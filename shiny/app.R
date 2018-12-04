#look in govnotes

library(shiny)
library(tidyverse)
library(dplyr)
library(tidytext)
library(shinydashboard)

# Read in Data for charts from RDS files

chart1 <- read_rds("chart1.rds")
chart2 <- read_rds("chart2.rds")
chart3 <- read_rds("chart3.rds")
chart4 <- read_rds("chart4.rds")
chart5 <- read_rds("chart5.rds")
chart6 <- read_rds("chart6.rds")
chart7 <- read_rds("chart7.rds")
chart8 <- read_rds("chart8.rds")


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ui <- dashboardPage(
  dashboardHeader(title = "American Dream"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Income Inequality", tabName = "inequality", icon = icon("line-chart")),
      menuItem("Income Bracket", tabName = "income", icon = icon("dollar")),
      menuItem("Time Period", tabName = "time", icon = icon("calendar")),
      menuItem("Explore the Data", tabName = "explore", icon = icon("globe"))
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
              p(paste("The two key elements we will focus on is the downward slope of the 
                fraction of children expected to earn more than their parents, and the increase
                in the disparity between this fraction for high and low income brackets."))
      ),
      
      # Second tab content
      tabItem(tabName = "inequality",
              h2(strong("Income Inequality as a Driving Force")),
              plotOutput("plot6"),
              p("We would expect that there would be a positive relationship between the two lines, as more income inequality
                should correlate with a higher dispairty in potential earnings between income brackets. The lack of correlation
                in this graph is becuase the data shows someone BORN that year's probability to make more than their parents current income. In
                the methodology section the author explains that he used the age bracket 25-32 to estimate the
                parent's income. So we will adjust the income inequality data by 28 years to match the conditions
                that a child born in a given year would experience when in the adult age group described."),
              plotOutput("plot7"),
              p("After adjusting for age of adulthood, the negative correlation between the two datasets confirms 
                that increasing levels of inequality is correlated with a lower percentage chance of making
                more than ones parents.")
      ),
      
      # Second tab content
      tabItem(tabName = "income",
              h2(strong("Lower Income Brackets are Adversely Affected")),
              plotOutput("plot2"),
              p("The above chart shows the differences in the change of fraction of children who make more than
                their parents by income bracket. It is clear that children from families with higher income brackets 
                almost always have a higher percentage chance of earning more than their parents than those
                from low income brackets. We can plot a few key comparisons below to examine how the relationship between
                the high and low income brackets changed over time."),
              plotOutput("plot3"),
              p("It is interesting to see the sharp increase in income disparities during the 1940 - 1954 
                time period, compared with the gradual slope from 1954 onwards (disparity decreases between the
                top percentile and the bottom percentile during this period). We will now compare this income disparity data, 
                specifically the data regarding the top 10 percentiles vs. the bottom 10 percentiles with public inequality
                data regarding the percentage of wealth held by the top 1% of the country."),
              plotOutput("plot4"),
              p("We would expect that there would be a positive relationship between the two lines, as more income inequality
                should correlate with a higher dispairty in potential earnings between income brackets. The lack of correlation
                in this graph is becuase the data shows someone BORN that year's probability to make more than their parents current income. In
                the methodology section the author explains that he used the age bracket 25-32 to estimate the
                parent's income. So we will adjust the income inequality data by 28 years to match the conditions
                that a child born in a given year would experience when in the adult age group described."),
              plotOutput("plot5"),
              p("By adjusting the inequality data we see a clear positive correlation between the two datasets.")
      ),
      
      tabItem(tabName = "time",
              h2(strong("Time"))
      ),
      
      tabItem(tabName = "explore",
              
              radioButtons("year", "Distribution type:",
                           c(1940, 1950, 1960, 1980)),
              
              h2(strong("Explore")),
              plotOutput("plot8")
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
    chart2 %>% 
      filter(income_bracket %% 5 == 0) %>% 
    ggplot(aes(x = cohort, 
                                  y = probability_exceed, 
                                  group = income_bracket, 
                                  col = income_bracket)) + 
      geom_line(size = .75, alpha = .4) + 
      geom_point(size = 2, alpha = .4) + 
      coord_cartesian(ylim = c(.0, 1.00)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_gradient(low = "green", high = "red") +
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
  
  # Chart 4.1 Plot
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
  
  
  # Chart 4.2 Plot
  output$plot5 <- renderPlot({
    chart5 %>% 
      ggplot(aes(x = year)) + 
      geom_line(aes(y = percentage.dif, colour = "Top vs. Bottom 10% Disparity")) +
      geom_line(aes(y = percentage.inequality * 2.5, colour = "Wealth Held by Top 1%")) +
      scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "% of Wealth Heald by top 1%")) +
      labs(title = "Disparities Between Top and Bottom 10th Percentile vs. Income Inequality (Inequality Data Adjusted)",
           y = "Dif. between top 10% percentile and bottom 10% percentile",
           x = "Year")
  })
  
  
  # Chart 6 Plot
  output$plot6 <- renderPlot({
  ggplot(chart6, aes(x = year)) +
    geom_line(aes(y = percentage.simple, colour = "Pct. of Children Earning More than their Parents"), size = .75) +
    geom_point(aes(y = percentage.simple, colour = "Pct. of Children Earning More than their Parents"), size = 1.5) +
    geom_line(aes(y = percentage.inequality * 6, colour = "Wealth Held by Top 1%"), size = .75) +
    geom_point(aes(y = percentage.inequality * 6, colour = "Wealth Held by Top 1%"), size = 1.5) +
    scale_y_continuous(sec.axis = sec_axis(~./6, name = "% of Wealth Heald by top 1%")) +
    labs(title = "Disparities Between Top and Bottom 10th Percentile vs. Income Inequality",
         y = "Dif. between top 10% percentile and bottom 10% percentile",
         x = "Year") +
    theme_bw() +
    theme(axis.title.x = element_text(colour = "#9d9eac"),
          axis.title.y = element_text(colour = "#9d9eac")) +
    theme(legend.position="bottom")
  })  
  
  # Chart 7 Plot
  output$plot7 <- renderPlot({
    ggplot(chart7, aes(x = year)) +
      geom_line(aes(y = percentage.simple, colour = "Pct. of Children Earning More than their Parents"), size = .75) +
      geom_point(aes(y = percentage.simple, colour = "Pct. of Children Earning More than their Parents"), size = 1.5) +
      geom_line(aes(y = percentage.inequality * 4, colour = "Wealth Held by Top 1%"), size = .75) +
      geom_point(aes(y = percentage.inequality * 4, colour = "Wealth Held by Top 1%"), size = 1.5) +
      scale_y_continuous(sec.axis = sec_axis(~./4, name = "% of Wealth Heald by top 1%")) +
      labs(title = "Disparities Between Top and Bottom 10th Percentile vs. Income Inequality",
           y = "Dif. between top 10% percentile and bottom 10% percentile",
           x = "Year") +
      theme_bw() +
      theme(axis.title.x = element_text(colour = "#9d9eac"),
            axis.title.y = element_text(colour = "#9d9eac")) +
      theme(legend.position="bottom")
  })
  
  output$plot8 <- renderPlot({
    chart8 %>% filter(cohort == input$year) %>% 
      ggplot() + 
      geom_polygon(aes(x = long, y = lat, fill = cohort_mean, group = group), color = "white") + 
      coord_fixed(1.3) +
      ditch_the_axes  +
      scale_fill_gradientn(colours = c("#ce2828", "#7a2416", "#14254f", "#1e6612", "#41ce28"),
                           limits = c(.3,1))
  })
  
}

shinyApp(ui, server)