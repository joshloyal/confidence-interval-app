library(shiny)
library(ggplot2)
library(tidyverse)
library(devtools)

devtools::install_github("joshloyal/loyalr")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  title = "Confidence Intervals",

  fluidRow(
    column(3,
      h4('Experiment Controls'),
      checkboxInput("show_intervals",
        "Show Confidence Intervals",
        FALSE),
      sliderInput("n_experiments",
        "Number of experiments:",
        min = 10,
        max = 100,
        step = 10,
        value = 20),
      sliderInput("n_samples",
        "Number of samples:",
        min = 10,
        max = 50,
        step = 5,
        value = 10),
      sliderInput("alpha",
        "Confidence Level (alpha):",
        min = 5,
        max = 100,
        step = 5,
        value = 95)
    ),
    column(8,
      plotOutput('IntervalPlot', height = '700px')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$IntervalPlot <- renderPlot({
    set.seed(123)

    show_intervals <- input$show_intervals
    n_experiments = input$n_experiments
    n_samples = input$n_samples
    alpha = 1 - (input$alpha / 100)

    true_mean <- 10
    true_standard_deviation <- 1

    sample_means <- vector('numeric', n_experiments)
    upper_cis <- vector('numeric', n_experiments)
    lower_cis <- vector('numeric', n_experiments)
    contains_true_mean <- 0
    for (i in 1:n_experiments) {
      sample <- true_standard_deviation * rnorm(n_samples) + true_mean
      sample_means[i] = mean(sample)

      sample_variance <- var(sample)
      margin_of_error <- qt(alpha/2, n_samples - 1, lower.tail = FALSE) * sample_variance / sqrt(n_samples)
      upper_cis[i] <- sample_means[i] + margin_of_error
      lower_cis[i] <- sample_means[i] - margin_of_error
      if (true_mean >= lower_cis[i] && true_mean <= upper_cis[i]) {
        contains_true_mean <- contains_true_mean + (1/n_experiments)
      }
    }

    confidence_text <- paste0(input$alpha, "%")
    title_text <- paste(confidence_text, "Confidence Intervals", sep = " ")
    sample_text <- paste0("# of samples per experiment: ", input$n_samples)
    subtitle_text <- sample_text
    if (show_intervals) {
      subtitle_text <- paste0(sample_text, '\t\t\t', 'Observed Coverage: ',
                              round(contains_true_mean, 2) * 100, '%')
    }

    p <- tibble(
      sample_mean = sample_means,
      upper_ci = upper_cis,
      lower_ci = lower_cis,
    ) %>%
      mutate(experiment_number = as.factor(row_number())) %>%
      ggplot(aes(x = experiment_number, y = sample_mean)) +
      geom_point(size = 3, color = 'steelblue') +
      geom_hline(aes(yintercept = true_mean), linetype = 'dashed', size = 1) +
      scale_y_continuous(limits = c(8, 12)) +
      coord_flip() +
      ggtitle(title_text, subtitle = subtitle_text) +
      loyalr::theme_pub() +
      theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

    if (show_intervals) {
      p <- p +
        geom_linerange(aes(ymin = lower_ci, ymax = upper_ci)) +
        geom_point(size = 3, color = 'steelblue')

    }
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
