#look in govnotes

library(shiny)
library(tidyverse)
library(tidytext)
library(shinydashboard)

# Read in Data for charts from RDS files

# chart 1
chart1 <- read_rds("chart1.rds")
chart2 <- read_rds("chart2.rds")



ui <- dashboardPage(
  dashboardHeader(title = "American Dream"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Income Bracket", tabName = "income", icon = icon("dollar"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2(strong("Analyzing 'The American Dream' study by Raj Chetty")),
              h3("Summary"),
              p("I aim to further analyze Raj Chetty's findings regarding the fraction of children
                that earn more than their parents in his 'American Dream' dataset."),
              h3("Background"),
              p("'The American Dream' study looks at the fraction of children who grow up to earn more than their parents
                as a means of analyzing the American Dream's evolution over time. They found that this 
                percentage has sharply declined from over 90% in 1940 to just around 50% today. They conclude
                that changes in the distribution of economics growth account for this change."),
              plotOutput("plot1"),
              h3("Findings"),
              p("Please use the tabs to the left to explore the data, and see our findings")
      ),
      
      # Second tab content
      tabItem(tabName = "income",
              h2(strong("The American Dream by Income")),
              plotOutput("plot2"),
              p("The above chart shows the differences in the change of fraction of children who make more than
                their parents by income bracket. It is clear that higher children from families with higher income brackets 
                almost always have a higher percentage chance of earning more than their parents than those
                from low income brackets. We can plot a few key comparisons below to further demonstrate these
                changes over time.")
      )
    )
  )
)

server <- function(input, output) {
  
  # Chart 1 Plot
  output$plot1 <- renderPlot({
    ggplot(simple_plot, aes(x = cohort, y = cohort_mean)) + 
      geom_line(size = .75) + geom_point(size = 3) + 
      coord_cartesian(ylim = c(.48, 1.00)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Child's Year of Birth",
           y = "Pct. of Children Earning More than their Parents",
           title = "Percent of Children Earning More than Their Parents, by Year of Birth") +
      theme_bw() +
      theme(axis.title.x = element_text(colour = "#9d9eac"),
            axis.title.y = element_text(colour = "#9d9eac"))
  })
  
  # Chart 1 Plot
  output$plot2 <- renderPlot({
    ggplot(chart2, aes(x = cohort, 
                                  y = probability_exceed, 
                                  group = income_bracket, 
                                  col = income_bracket)) + 
      geom_line(size = .75, alpha = .1) + 
      geom_point(size = 2, alpha = .1) + 
      coord_cartesian(ylim = c(.0, 1.00)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_gradient(low = "green", high = "black") +
      labs(x = "Child's Year of Birth",
           y = "Pct. of Children Earning More than their Parents",
           title = "Percent of Children Earning More than Their Parents, by Year of Birth") +
      theme_bw() +
      theme(axis.title.x = element_text(colour = "#9d9eac"),
            axis.title.y = element_text(colour = "#9d9eac"))
  })
  
}

shinyApp(ui, server)