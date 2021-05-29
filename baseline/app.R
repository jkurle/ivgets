# the following only needs to be loaded once on the server

library(shiny)
library(ivgets)
library(ggplot2)
library(stats)
library(stringr)
# at the moment, data stays the same and user cannot re-generate new random data
# reason is that this would require the r2sls package, which is not yet released
set.seed(14)

# formula stays the same
fml <- y ~ -1+x1+x2+x11 | -1+x1+x2+z11+z12

# user interface
ui <- fluidPage(

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
  actionButton(inputId = "run", label = "Run"),

  plotOutput("errors"),
  verbatimTextOutput("detected")


)

server <- function(input, output) {

  # generate new outliers according to settings when "generate" pressed
  df <- eventReactive(input$generate, {

    data <- ivgets::artificial2sls_shiny

    outliers <- sample(1:100, size = input$share * 100, replace = FALSE)
    outliers <- outliers[order(outliers)]

    size <- sample(c(input$magnitude, -input$magnitude),
                   size = length(outliers), replace = TRUE)

    data[outliers, "y"] <- data[outliers, "y"] - data[outliers, "u"] + size
    data[outliers, "u"] <- data[outliers, "u"] - data[outliers, "u"] + size

    data[outliers, "is.outlier"] <- 1

    return(data)

  })

  critical <- eventReactive(input$generate, {
    qnorm(p = input$tpval/2, lower.tail = FALSE)
  })

  ylimits <- eventReactive(input$generate, {
    lim <- max(critical(), input$magnitude)
    c(-lim, lim)
  })

  output$errors <- renderPlot({
    orig.data <- artificial2sls
    orig.data$is.outlier <- 0
    orig.data$is.outlier <- factor(orig.data$is.outlier, levels = c("0", "1"))
    uscatter <- ggplot(data = orig.data) +
      geom_point(aes(x = id, y = u, color = is.outlier), size = 2) +
      scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
      coord_cartesian(xlim = c(0, 100), ylim = c(-2.576, 2.576)) +
      ggtitle(label = "True Errors") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab(label = "Index") + ylab(label = "Magnitude of Error") +
      guides(colour = guide_legend(title = "Outlier")) +
      geom_hline(aes(yintercept = -2.576, lty = "Lower")) +
      geom_hline(aes(yintercept = 2.576, lty = "Upper")) +
      scale_linetype_manual(name = "Cutoff", values = c("dashed", "dashed"))
    return(uscatter)
  })

  output$errors <- renderPlot({

    # if(input$generate == 0) {
    #   sudata <- artificial2sls
    #   sudata$id <- 1:100
    #   sudata$is.outlier <- 0
    #   sudata$is.outlier <- as.factor(sudata$is.outlier)
    #   uscatter <- ggplot(data = sudata) +
    #     geom_point(aes(x = id, y = u, color = is.outlier), size = 2) +
    #     scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
    #     coord_cartesian(xlim = c(0, 100), ylim = c(-2.576, 2.576)) +
    #     ggtitle(label = "True Errors") +
    #     theme(plot.title = element_text(hjust = 0.5)) +
    #     xlab(label = "Index") + ylab(label = "Magnitude of Error") +
    #     guides(colour = guide_legend(title = "Outlier")) +
    #     geom_hline(aes(yintercept = -2.576, lty = "Lower")) +
    #     geom_hline(aes(yintercept = 2.576, lty = "Upper")) +
    #     scale_linetype_manual(name = "Cutoff", values = c("dashed", "dashed"))
    #   return(uscatter)
    # } else {
      uscatter <- ggplot(data = df()) +
        geom_point(aes(x = id, y = u, color = is.outlier), size = 2) +
        scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
        coord_cartesian(xlim = c(0, 100), ylim = ylimits()) +
        ggtitle(label = "True Errors") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab(label = "Index") + ylab(label = "Magnitude of Error") +
        guides(colour = guide_legend(title = "Outlier")) +
        geom_hline(aes(yintercept = -critical(), lty = "Lower")) +
        geom_hline(aes(yintercept = critical(), lty = "Upper")) +
        scale_linetype_manual(name = "Cutoff", values = c("dashed", "dashed"))
      return(uscatter)
    # }


    # if (input$run > 0) {
    #   selectnames <- model()$selection$ISnames
    #   selectnum <- as.numeric(stringr::str_extract(string = selectnames,
    #                                                pattern = "[1-9]([0-9]+)?"))
    #   selectdf <- data.frame(df()[selectnum, "u", drop = FALSE])
    #   selectdf$y2 <- selectdf$u
    #   selectdf$u <- NULL
    #   selectdf$y1 <- 0
    #   selectdf$x1 <- selectnum
    #   selectdf$x2 <- selectnum
    #   rev <- uscatter +
    #     geom_segment(data = selectdf, aes(x = x1, y = y1, xend = x2, yend = y2),
    #                  colour = "green") +
    #     scale_linetype_manual("Detected", values = c("Detected" = 2))
    #   return(rev)
    #}

  })

  # run model when press "run"
  model <- eventReactive(input$run, {
    isatmodel <- ivisat(formula = fml, data = df(),
                        iis = ("IIS" %in% input$indicators),
                        sis = ("SIS" %in% input$indicators),
                        t.pval = input$tpval, print.searchinfo = FALSE)
  })

  output$detected <- renderPrint(model()$selection$ISnames)

}

shinyApp(ui = ui, server = server)
