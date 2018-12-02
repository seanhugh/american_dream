#look in govnotes

library(shiny)
library(tidyverse)
library(tidytext)
library(shinydashboard)

# Read in Data for charts from RDS files

chart1 <- read_rds("chart1.rds")
chart2 <- read_rds("chart2.rds")
chart3 <- read_rds("chart3.rds")
chart4 <- read_rds("chart4.rds")



ui <- dashboardPage(
  dashboardHeader(title = "American Dream"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Income Bracket", tabName = "income", icon = icon("dollar")),
      menuItem("Time Period", tabName = "time", icon = icon("calendar"))
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
                changes over time."),
              plotOutput("plot3"),
              p("It is interesting to see the sharp increase in income disparities during the 1940 - 1954 
                time period, compared with the gradual slope from 1954 onwards (disparity decreases between the
                top percentile and the bottom percentile during this period)."),
              plotOutput("plot4")
      )
    )
  )
)

server <- function(input, output) {
  
  # Chart 1 Plot
  output$plot1 <- renderPlot({
    ggplot(chart1, aes(x = cohort, y = cohort_mean)) + 
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
  
  # Chart 2 Plot
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
  
  # Chart 3 Plot
  output$plot3 <- renderPlot({
    ggplot(chart3, aes(x = cohort, y = data, color = Percentile)) + 
      geom_line(size = 1) +
      labs(title = "Disparities Between Percentage Chances of Earning More than Parents",
           y = "Dif. between top x percentile and bottom x percentile",
           x = "Year")
  })
  
  # Chart 4 Plot
  output$plot4 <- renderPlot({
    chart4 %>% 
      ggplot(aes(x = year)) + 
      geom_line(aes(y = percentage.dif, colour = "Top vs. Bottom 10% Disparity")) +
      geom_line(aes(y = percentage.inequality * 2.5, colour = "Wealth Held by Top 1%")) +
      scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "% of Wealth Heald by top 1%")) +
      labs(title = "Disparities Between Top and Bottom 10th Percentile vs. Income Inequality",
           y = "Dif. between top 10% percentile and bottom 10% percentile",
           x = "Year")
  })
  
}

shinyApp(ui, server)