# Title: Retirement Contribution Calculator 
# Description: This calculates how much money one will have for retirement after
# a set number of years given initial inputs. This is a rough estimate that 
# shows growth and made money over time. 
# Author: Andrew Wapperom
# Date: 11/26/2023


# =======================================================
# Packages
# =======================================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets

# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Retirement Contribution Calculator"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      numericInput(inputId = "initialSalary",
                  label = "Initial Salary:",
                  value = 80000),
      numericInput(inputId = "rateOfGrowth",
                   label = "Rate Of Growth:",
                   value = 0.02),
      numericInput(inputId = "contributionPercentage",
                   label = "Contribution Percentage:",
                   value = 0.15),
      numericInput(inputId = "numPeriods",
                   label = "Number of Periods",
                   value = 12),
      numericInput(inputId = "yearsInvested",
                   label = "Years Invested:",
                   value = 5),
      numericInput(inputId = "annualROR",
                   label = "Annual Rate of Return:",
                   value = 0.08),
      numericInput(inputId = "targetAmount",
                   label = "Target Amount:",
                   value = 35000),
      checkboxInput(inputId = "showTarget",
                   label = "Show Target:",
                   value = FALSE),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("Plot 1"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("Plot 2"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("Table"),
      dataTableOutput(outputId = "table"),
    )
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive Balance table
  # ------------------------------------------------------------
  tbl = reactive({
    years = 1:input$yearsInvested
    g = input$rateOfGrowth
    p = input$contributionPercentage
    k = input$numPeriods
    n = input$yearsInvested
    r = input$annualROR
    
    
    t = 0
    bal_0 = 0
    bal = double(n*k)
    S = double(n)
    C = double(n)
    end_of_year_bal = double(n)
    
    for (year in 1:input$yearsInvested) {
      if (year == 1) {
        S[1] = input$initialSalary
      } else {
        S[year] = S[year - 1] * (1 + g)
      }
      C[year] = S[year] * (p/k)
      for (j in 1:k) {
        t = t + 1
        bal_0 = bal_0 * (1 + r/k) + C[year]
      }
      end_of_year_bal[year] = bal_0
    }
    
    balance_tbl = data.frame(
      year = years,
      salary = S,
      balance = end_of_year_bal
    )
    
    balance_tbl = balance_tbl %>% mutate (
      annual_contrib = salary * p,
      own = cumsum(annual_contrib),
      growth = balance - own,
      own_pct = own * 100 / balance,
      growth_pct = growth * 100 / balance,
      hit_target = ifelse(balance >= input$targetAmount, "yes", "no")
      
    )

    
    balance_tbl
  })
  
  
  # ------------------------------------------------------------
  # Plot of balance timeline
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    plot = ggplot(data = tbl(), aes(x = year, y = balance)) +
      geom_line() + 
      geom_area(fill = "lightblue") + 
      scale_y_continuous(label = scales::comma)
    if (input$showTarget) {
      plot = plot + 
        geom_hline(yintercept = input$targetAmount, linetype = "dashed", color = "red")
    }
    plot
  })

  
  # ------------------------------------------------------------
  # Plot of balance decomposition
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    df = tbl() |> 
      select(year, own, growth) %>%
      pivot_longer(cols = own:growth, names_to = "type")
    
    ggplot(data = df, aes(x = year, y = value, fill = type)) + 
      geom_col()
    
  })

  
  # ------------------------------------------------------------
  # Table with Retirement Balance data
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    # to limit the number of decimal digits in the rendered table
    # you can convert it into a "DataTable" object, and then use
    # formatRound() to limit the displayed decimals.
    tbl() |>
      datatable() |>
      formatRound(columns = c("salary", "annual_contrib", "balance", "own", "growth", "own_pct", "growth_pct"), 
                        digits = 2) # round to 2-digits
    
    
  })
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
