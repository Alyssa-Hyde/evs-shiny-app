# ============================================
# app.R — EVS Shiny App (Final Assignment)
# ============================================

library(shiny)
library(dplyr)
library(ggplot2)
library(broom)
library(knitr)

# ── Load data ──────────────────────────────────────────────────────────────────
evs <- readRDS("data/evs_clean.rds")

# Country choices: "Overall" + sorted country list
country_choices <- c("Overall", sort(unique(evs$country)))

# Outcome choices
outcome_choices <- c(
  "Child suffers when mother works" = "v72",
  "Job should go to national"       = "v80"
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("EVS Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # (a) Country
      selectInput("country", "Country",
                  choices  = country_choices,
                  selected = "Overall"),
      
      # (b) Outcome
      radioButtons("outcome", "Outcome variable",
                   choices  = outcome_choices,
                   selected = "v72"),
      
      hr(),
      
      # (c) Controls (multiple choice)
      checkboxGroupInput("controls", "Additional controls",
                         choices  = c("Sex" = "sex", "Education" = "edu"),
                         selected = NULL),
      
      # (d) Age polynomial
      numericInput("poly_age", "Age polynomial degree",
                   value = 1, min = 1, max = 5, step = 1),
      
      hr(),
      
      # Download report button
      downloadButton("download_report", "Download HTML Report",
                     class = "btn-success btn-block")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",
                  
                  # ── 1. Overview ──────────────────────────────────────────────────────
                  tabPanel("Overview",
                           br(),
                           h3("Welcome to the EVS Data Explorer"),
                           p("This app allows you to interactively explore data from the ",
                             strong("European Values Study (EVS)"), ", focusing on two attitudinal outcomes:"),
                           tags$ul(
                             tags$li(strong("Child suffers when mother works (v72):"),
                                     " Agreement with the statement that a child suffers when
                     the mother works full time. Scale: 1 (strongly agree)
                     to 4 (strongly disagree)."),
                             tags$li(strong("Job should go to national (v80):"),
                                     " Agreement with the statement that employers should give
                     priority to nationals over immigrants. Scale: 1 (strongly agree)
                     to 4 (strongly disagree).")
                           ),
                           h4("How to navigate"),
                           tags$ol(
                             tags$li(strong("Sidebar inputs"), " — Filter by country, choose an outcome,
                    add control variables (sex, education), and adjust the polynomial
                    degree for age."),
                             tags$li(strong("Exploration tab"), " — View distributions of the selected
                    outcome, age, education and sex for the chosen country/sample."),
                             tags$li(strong("Regression tab"), " — Inspect regression coefficients and
                    a residual diagnostic plot based on your selected model."),
                             tags$li(strong("Download HTML Report"), " — Export a self-contained report
                    reflecting all current selections.")
                           ),
                           p("Use the ", strong("Country"), " selector to restrict all outputs to a
            single country, or leave it on 'Overall' to use the full sample."),
                           hr(),
                           p(em("Data source: European Values Study (EVS) Wave 5 (ZA7500_v5-0-0)."))
                  ),
                  
                  # ── 2. Exploration ───────────────────────────────────────────────────
                  tabPanel("Exploration",
                           br(),
                           h4("Outcome variable"),
                           plotOutput("plot_outcome", height = "250px"),
                           hr(),
                           h4("Controls"),
                           fluidRow(
                             column(4, plotOutput("plot_age",  height = "220px")),
                             column(4, plotOutput("plot_edu",  height = "220px")),
                             column(4, plotOutput("plot_sex",  height = "220px"))
                           )
                  ),
                  
                  # ── 3. Regression ────────────────────────────────────────────────────
                  tabPanel("Regression",
                           br(),
                           h4("Model formula"),
                           verbatimTextOutput("formula_display"),
                           hr(),
                           h4("Coefficients"),
                           tableOutput("reg_table"),
                           hr(),
                           h4("Predicted values vs. Residuals"),
                           plotOutput("plot_resid", height = "350px")
                  )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive: filtered data
  dat <- reactive({
    if (input$country == "Overall") evs else filter(evs, country == input$country)
  })
  
  # Reactive: outcome label for plots
  outcome_label <- reactive({
    names(outcome_choices)[outcome_choices == input$outcome]
  })
  
  # Reactive: regression formula string
  reg_formula_str <- reactive({
    poly_term <- if (input$poly_age == 1) "age" else paste0("poly(age, ", input$poly_age, ")")
    controls  <- input$controls   # character vector, may be NULL
    rhs       <- paste(c(poly_term, controls), collapse = " + ")
    paste0(input$outcome, " ~ ", rhs)
  })
  
  # Reactive: fitted model
  model <- reactive({
    lm(as.formula(reg_formula_str()), data = dat())
  })
  
  # ── Exploration plots ──────────────────────────────────────────────────────
  
  output$plot_outcome <- renderPlot({
    ggplot(dat(), aes(x = .data[[input$outcome]])) +
      geom_histogram(bins = 4, fill = "#2c7bb6", colour = "white") +
      scale_x_continuous(breaks = 1:4) +
      labs(title = outcome_label(), x = "Response (1–4)", y = "Count") +
      theme_minimal(base_size = 13)
  })
  
  output$plot_age <- renderPlot({
    ggplot(dat(), aes(x = age)) +
      geom_histogram(bins = 30, fill = "#1a9641", colour = "white") +
      labs(title = "Age", x = "Age", y = "Count") +
      theme_minimal(base_size = 13)
  })
  
  output$plot_edu <- renderPlot({
    ggplot(dat(), aes(x = edu)) +
      geom_bar(fill = "#d7191c") +
      labs(title = "Education", x = "Education level", y = "Count") +
      theme_minimal(base_size = 13)
  })
  
  output$plot_sex <- renderPlot({
    ggplot(dat(), aes(x = sex)) +
      geom_bar(fill = "#fdae61") +
      labs(title = "Sex", x = "Sex", y = "Count") +
      theme_minimal(base_size = 13)
  })
  
  # ── Regression outputs ─────────────────────────────────────────────────────
  
  output$formula_display <- renderText({
    reg_formula_str()
  })
  
  output$reg_table <- renderTable({
    tidy(model(), conf.int = TRUE) |>
      mutate(across(where(is.numeric), \(x) round(x, 4))) |>
      rename(Term = term, Estimate = estimate, `Std. Error` = std.error,
             Statistic = statistic, `p-value` = p.value,
             `CI low` = conf.low, `CI high` = conf.high)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$plot_resid <- renderPlot({
    df <- data.frame(
      fitted    = fitted(model()),
      residuals = residuals(model())
    )
    ggplot(df, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.3, colour = "#2c7bb6") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
      geom_smooth(method = "loess", se = FALSE, colour = "#d7191c") +
      labs(title = "Predicted vs. Residuals",
           x = "Fitted values", y = "Residuals") +
      theme_minimal(base_size = 13)
  })
  
  # ── Download report ────────────────────────────────────────────────────────
  
  output$download_report <- downloadHandler(
    filename = "evs_report.html",
    content  = function(file) {
      # Copy report template to a temp dir so knitr can write there
      tmpdir   <- tempdir()
      tmp_rmd  <- file.path(tmpdir, "report.Rmd")
      file.copy("report.Rmd", tmp_rmd, overwrite = TRUE)
      
      params <- list(
        country       = input$country,
        outcome       = input$outcome,
        outcome_label = outcome_label(),
        controls      = input$controls,
        poly_age      = input$poly_age,
        formula_str   = reg_formula_str(),
        data          = dat()
      )
      
      rmarkdown::render(
        input       = tmp_rmd,
        output_file = file,
        params      = params,
        envir       = new.env(parent = globalenv()),
        quiet       = TRUE
      )
    }
  )
}

shinyApp(ui, server)