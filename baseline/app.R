# the following only needs to be loaded once on the server

library(shiny)
library(ivgets)
library(ggplot2)
library(stats)
library(stringr)
library(waiter)
# at the moment, data stays the same and user cannot re-generate new random data
# reason is that this would require the r2sls package, which is not yet released
set.seed(14)

# formula stays the same
fml <- y ~ -1+x1+x2+x11 | -1+x1+x2+z11+z12

# load data
original <- artificial2sls_shiny


# user interface
ui <- fluidPage(

  titlePanel("Illustration of IIS in 2SLS Models"),

  sidebarLayout(

    sidebarPanel(

      sliderInput(inputId = "share", label = "Share of outliers", value = 0.05,
                  min = 0, max = 0.2, step = 0.01),
      sliderInput(inputId = "magnitude", label = "Outlier magnitude", value = 3,
                  min = 2, max = 6, step = 0.5),
      numericInput(inputId = "negative", label = "Probability of negative outlier",
                   value = 0.5, min = 0, max = 1),
      numericInput(inputId = "tpval", label = "Significance level for selection",
                   value = 0.01, min = 0, max = 0.2),
      checkboxGroupInput(inputId = "indicators", label = "Type of indicators",
                         choices = list("impulse indicator saturation (IIS)" = "IIS",
                                        "step indicator saturation (SIS)" = "SIS"),
                         selected = "IIS"),
      actionButton(inputId = "generate", label = "Generate outliers"),
      actionButton(inputId = "run", label = "Run")

    ),

    mainPanel(

      fluidRow(
        plotOutput("errors")
      ),
      fluidRow(
        column(width = 5,
          tableOutput("performance")
        ),
        column(width = 7,
          textOutput("outliers"),
          textOutput("detected")
        )
      )

    )
  )
)

server <- function(input, output) {

  tracker <- reactiveVal(0)

  # translate t.pval for selection into critical value / cutoff
  critical <- eventReactive(input$generate, {
    qnorm(p = input$tpval/2, lower.tail = FALSE)
  })
  # graph should always show both the outliers and the cutoff
  ylimits <- eventReactive(input$generate, {
    lim <- max(critical(), input$magnitude)
    c(-lim, lim)
  })

  # generate new outliers whenever press "generate"
  outliers <- eventReactive(input$generate, {

    tracker(0)

    # sample random indices and order them
    indices <- sample(1:100, size = input$share * 100, replace = FALSE)
    indices <- indices[order(indices)]

    # sample how large they should be
    size <- sample(c(input$magnitude, -input$magnitude),
                   size = length(indices), replace = TRUE,
                   prob = c(1-input$negative, input$negative))

    outliers <- list(indices = indices, size = size)
    return(outliers)

  })
  # update data continously
  # but since outliers only regenerated when press button, only then updated
  df <- reactive({

    data <- original
    if (input$generate == 0) {

    } else {
      ind <- outliers()$indices
      sz <- outliers()$size
      data[ind, "y"] <- data[ind, "y"] - data[ind, "u"] + sz
      data[ind, "u"] <- data[ind, "u"] - data[ind, "u"] + sz
      data[ind, "is.outlier"] <- 1
    }
    return(data)
  })

  # run model when press "run"
  model <- eventReactive(input$run, {

    newval <- tracker() + 1
    tracker(newval)

    # give notification while running
    id1 <- showNotification("Estimating model...", duration = NULL,
                            closeButton = FALSE, type = "message")
    on.exit(removeNotification(id1), add = TRUE)

    isatmodel <- ivisat(formula = fml, data = df(),
                        iis = ("IIS" %in% input$indicators),
                        sis = ("SIS" %in% input$indicators),
                        t.pval = input$tpval, print.searchinfo = FALSE)

  })

  perform <- reactive({

    if (input$run == 0 || tracker() == 0) {
      potency <- NA
      gauge <- NA
    } else {
      det.names <- model()$selection$ISnames
      det <- as.numeric(str_extract(string = det.names,
                                    pattern = "[1-9]([0-9]+)?"))

      # isolate outliers so that if after model has run, someone generates new
      # outliers it does not immediately calculate potency again
      potency <- sum(det %in% isolate(outliers()$indices)) / length(isolate(outliers()$indices))
      gauge <- sum(!(det %in% isolate(outliers()$indices))) / length(det)
    }

    # store result in a matrix
    out <- matrix(c(gauge, potency), nrow = 2, ncol = 1)
    rownames(out) <- c("Gauge", "Potency")
    colnames(out) <- "Performance"
    return(out)

  })




  # create matrix for detected outlier lines
  iislines <- reactive({

    if (input$run == 0 || tracker() == 0) {
      selectdf <- data.frame(matrix(c(0,0,0,0), ncol = 4, nrow = 1))
      colnames(selectdf) <- c("x1", "y1", "x2", "y2")
    } else {
      selectnames <- model()$selection$ISnames
      selectnum <- as.numeric(str_extract(string = selectnames, pattern = "[1-9]([0-9]+)?"))
      selectdf <- data.frame(isolate(df()[selectnum, "u", drop = FALSE]))
      selectdf$y2 <- selectdf$u
      selectdf$u <- NULL
      selectdf$y1 <- 0
      selectdf$x1 <- selectnum
      selectdf$x2 <- selectnum
    }
    return(selectdf)

  })

  ### output
  baseplot <- reactive({

    if (input$generate == 0) {
      base <- ggplot(data = df()) +
        geom_point(aes(x = id, y = u, color = is.outlier), size = 2) +
        scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
        coord_cartesian(xlim = c(0, 100), ylim = c(-3, 3)) +
        ggtitle(label = "True Errors") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab(label = "Index") + ylab(label = "Magnitude of Error") +
        guides(colour = guide_legend(title = "Outlier")) +
        geom_hline(aes(yintercept = -qnorm(p = 0.01/2, lower.tail = FALSE), lty = "Lower")) +
        geom_hline(aes(yintercept = qnorm(p = 0.01/2, lower.tail = FALSE), lty = "Upper")) +
        scale_linetype_manual(name = "Cutoff", values = c("dashed", "dashed"))
    } else {
      base <- ggplot(data = df()) +
        geom_point(aes(x = id, y = u, color = is.outlier), size = 2) +
        scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
        coord_cartesian(xlim = c(0, 100), ylim = ylimits()) +
        ggtitle(label = "True Errors") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab(label = "Index") + ylab(label = "Magnitude of Error") +
        guides(colour = guide_legend(title = "Outlier")) +
        geom_hline(aes(yintercept = -critical(), lty = "Lower")) +
        geom_hline(aes(yintercept = critical(), lty = "Upper")) +
        scale_linetype_manual(name = "Cutoff", values = c("dashed", "dashed")) +
        geom_segment(data = iislines(), aes(x = x1, y = y1, xend = x2, yend = y2), color = "grey")
    }

    return(base)

  })


  output$errors <- renderPlot({

    return(baseplot())

  })

  output$performance <- renderTable({

    perform()

  }, rownames = TRUE, colnames = TRUE, bordered = TRUE, hover = TRUE,
  na = "no results", spacing = "xs")


  found <- reactive({
    det.names <- model()$selection$ISnames
    det <- as.numeric(str_extract(string = det.names,
                                  pattern = "[1-9]([0-9]+)?"))
    return(det)
  })

  output$outliers <- renderText({
    if (input$generate == 0) {
      paste("Outliers included: none", sep = "")
    } else {
      a <- paste(outliers()$indices, collapse = ", ")
      b <- paste("Outliers included: ", a, sep = "")
    }
  })
  output$detected <- renderText({
    detreactive()
  })


  fix <- eventReactive(input$run, {isolate(input$generate)})



  detreactive <- reactive({
    if (input$run == 0 || input$generate > fix()) {
      b <- paste("Outliers detected: no results", sep = "")
    } else {
      a <- paste(found(), collapse = ", ")
      b <- paste("Outliers detected: ", a, sep = "")
    }
    return(b)
  })

}

shinyApp(ui = ui, server = server)
