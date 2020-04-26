

# Using the /all route from https://covid19api.com/#details, then converting JSON to data frame

COVIDALLDATA <- fromJSON(txt = "all4232020.json")
COVIDALLDATASUMMARY <- fromJSON(txt = "summary4232020.json")
COVIDALLDATADF <- as.data.frame(COVIDALLDATA)
COVIDALLDATADF$Date <- as.Date(COVIDALLDATADF$Date)
COVIDALLDATADF$Lat <- as.numeric(COVIDALLDATADF$Lat)
COVIDALLDATADF$Lon <- as.numeric(COVIDALLDATADF$Lon)
COVIDALLDATADF <- COVIDALLDATADF %>%
  select(-"CityCode")

# Selecting top 8 countries by total cases, renaming, and aggregating for use in ggplot

COVIDALLDATADF_ggplot <- subset(COVIDALLDATADF, Country %in% c("United States of America", "Italy", "Spain", "China", "Germany",  "Iran, Islamic Republic of", "France", "United Kingdom"))
COVIDALLDATADF_ggplot$Country <- gsub("United States of America", "US", COVIDALLDATADF_ggplot$Country)
COVIDALLDATADF_ggplot$Country <- gsub("Iran, Islamic Republic of", "Iran", COVIDALLDATADF_ggplot$Country)
COVIDALLDATADF_ggplot$Country <- gsub("United Kingdom",  "UK", COVIDALLDATADF_ggplot$Country)
COVIDALLDATADF_ggplot_agg <- aggregate(COVIDALLDATADF_ggplot$Confirmed, by=list(Category=COVIDALLDATADF_ggplot$Date, COVIDALLDATADF_ggplot$Country), FUN=sum)
names(COVIDALLDATADF_ggplot_agg) = c("Date", "Country", "Cases")

# Reading/filtering/renaming summary data to use for Data Explorer. Used /Summary route from API and then converting JSON to data frame

COVIDALLDATASUMMARYDF <- as.data.frame(COVIDALLDATASUMMARY)
COVIDALLDATASUMMARYDF <- COVIDALLDATASUMMARYDF %>%
  select(-"Global.NewConfirmed", -"Global.TotalConfirmed", -"Global.NewDeaths", -"Global.TotalDeaths", -"Global.NewRecovered", -"Global.TotalRecovered", -"Countries.CountryCode", -"Countries.Slug", -"Countries.Date") 
COVIDALLDATASUMMARYDF <- COVIDALLDATASUMMARYDF %>%
  rename("Country" = "Countries.Country", "NewConfirmed" = "Countries.NewConfirmed", "TotalConfirmed" = "Countries.TotalConfirmed", "NewDeaths" = "Countries.NewDeaths", "TotalDeaths"  = "Countries.TotalDeaths", "NewRecovered" = "Countries.NewRecovered", "TotalRecovered" = "Countries.TotalRecovered")

# creating leaflet basemap

basemap2 = leaflet(COVIDALLDATADF) %>%
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-70, -120, ~80, 80)

# Creating interactive plot for Data Explorer tab

ggplot4 <- ggplot(COVIDALLDATADF_ggplot_agg, aes(x = Date, y = Cases, color = Country)) +
  geom_line(size = 0.8) +
  geom_point(size = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  theme_fivethirtyeight() +
  scale_color_tableau("Tableau 10")
fig3 <- ggplotly(ggplot4)
fig3 <- style(fig3, hoverinfo = "Cases", traces = 1)
fig3


# ggplot function for sidebar graph on Graph tab

ggplot1_functionnew = function(COVIDALLDATADF_ggplot_agg, date_range) {
  plot_dfnew = subset(COVIDALLDATADF_ggplot_agg, Date <= date_range)
  ggplot1new <- ggplot(plot_dfnew, aes(x = Date, y = Cases, color = Country)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.2) +
    scale_y_continuous(name = "Cumulative Cases", labels = scales::comma) +
    theme_few() +
    scale_color_tableau("Tableau 10")
  ggplot1new
}



# Start of ui and server for R Shiny

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Explorer", titleWidth = 205),
  dashboardSidebar(
    width = 205,
    sidebarMenu(
      menuItem(h4("Graph"),  tabName = "graph"),
      menuItem(h4("Data Explorer"), tabName = "rawdata")
    ),
    tags$i(h6("Data updated daily from the Johns Hopkins University CSSE repository, via covid19api.com. See more here: ", tags$a(href="https://github.com/CSSEGISandData/COVID-19", "JHU Coronavirus Data Repository"), "and", tags$a(href="https://covid19api.com/", "Covid-19 API")))
  ),
  #    plotOutput("ggplot1_function", height = "240px", width="100%")),
  dashboardBody(
    tabItems(
      tabItem(tabName = "graph",
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("mymap2", width = "100%", height = "1000px"))),
                column(width = 3,
                       box(width = NULL, status = "primary",
                           h3(textOutput("count_total_cases_reactivenew"), align = "center"))),
                column(width = 3,
                       box(width = NULL, status = "success",
                           h3(textOutput("count_recovered_cases_reactivenew"), align = "center"))),
                column(width = 3,
                       box(width = NULL, status = "danger",
                           h3(textOutput("count_death_cases_reactivenew"), align = "center"))),
                column(width = 3,
                       box(width = NULL, status = NULL,
                           tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
                           sliderInput("date_range", "Select Date Range", min(COVIDALLDATADF$Date),  max(COVIDALLDATADF$Date),
                                       value = max(COVIDALLDATADF$Date), animate = animationOptions(interval = 500, loop  = FALSE)))),
                column(width = 3,
                       box(width = NULL, status = NULL,
                           plotOutput("ggplot1_functionnew", height = "210px", width="100%")))
              )
      ),
      tabItem(tabName = "rawdata",
              fluidRow(
                DT::dataTableOutput("dataexplorernew"),
                plotlyOutput("ggplot4")
              )
              
              
      )
    ),
    useShinyalert()
  )
)



server <- function(input, output) {
  
  # Creating reactive data so that values change when new input is selected
  COV19_API_DF_Reactnew <- reactive({
    req(input$date_range)
    COVIDALLDATADF %>%
      filter(Date %in% input$date_range, Confirmed > 0)
  })
  
  COV19_API_DF_React_deathsnew <- reactive({
    COVIDALLDATADF %>%
      filter(Date %in% input$date_range) %>%
      select("Country", "Deaths", "Date")
  })
  
  COV19_API_DF_React_recoverednew <- reactive({
    COVIDALLDATADF %>%
      filter(Date %in% input$date_range) %>%
      select("Country", "Recovered", "Date")
  })
  
  # Leaflet base map output 
  output$mymap2 <- renderLeaflet({
    basemap2
    
  })
  
  # Leaflet map output with circle markers, labels, etc
  observeEvent(input$date_range, {
    leafletProxy("mymap2") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = COV19_API_DF_Reactnew(),
                       lat=~Lat,
                       lng=~Lon,
                       color = "#f08181",
                       radius = ~(Confirmed^(1/5)),
                       weight = 0,
                       fillOpacity = 0.5,
                       label = sprintf("<strong>%s<br/><strong>%s</strong><br/>Confirmed Cases: %s<br/>", COV19_API_DF_Reactnew()$Province, COV19_API_DF_Reactnew()$Country, prettyNum(COV19_API_DF_Reactnew()$Confirmed, big.mark= ",", scientific=FALSE)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3 px 8 px", color = "#f76363", "font-family" = "serif"),
                         textsize = "15px", direction = "auto",
                         opacity = 0.8
                       ))
    
  })
  
  # Displays reactive values for Confirmed Cases, Deaths, Recovered Cases, and ggplot in sidebar of "Graph" tab  
  
  output$count_total_cases_reactivenew <- renderText(({
    paste0(prettyNum(sum(COV19_API_DF_Reactnew()$Confirmed), big.mark = ","), " Confirmed Cases")
  }))
  
  output$count_death_cases_reactivenew <- renderText(({
    paste0(prettyNum(sum(COV19_API_DF_React_deathsnew()$Deaths), big.mark = ","), " Deaths")
  }))
  
  output$count_recovered_cases_reactivenew <- renderText(({
    paste0(prettyNum(sum(COV19_API_DF_React_recoverednew()$Recovered), big.mark = ","), " Recovered Cases")
  }))
  
  output$ggplot1_functionnew <- renderPlot({
    ggplot1_functionnew(COVIDALLDATADF_ggplot_agg, input$date_range)
  })
  
  # Displays reactive plot and DataTable for the "Data Explorer" tab
  
  output$ggplot4 <- renderPlotly({
    ggplot4
  })
  
  output$dataexplorernew <- DT::renderDataTable({
    df <- COVIDALLDATASUMMARYDF %>%
      select(Country, "Total Confirmed Cases" = "TotalConfirmed", "New Confirmed Cases" = "NewConfirmed",
             "Total Deaths" = "TotalDeaths", "New Deaths" = "NewDeaths", "Total Recovered Cases" = "TotalRecovered",
             "New Recovered Cases" = "NewRecovered")
    datatable(df, list(order = list(list(2, 'desc')))) %>%
      formatCurrency(c('Total Confirmed Cases', 'New Confirmed Cases', 'Total Deaths', 'New Deaths', 'Total Recovered Cases', 'New Recovered Cases'), currency = "", interval = 3, mark = ",") %>%
      formatRound(c('Total Confirmed Cases', 'New Confirmed Cases', 'Total Deaths', 'New Deaths', 'Total Recovered Cases', 'New Recovered Cases'), 0)
    
  })
  
  # Displays alert when app is opened
  
  shinyalert(
    title = "Welcome to the Coronavirus Explorer",
    text = "Click the play button on 'Select Date Range' to animate Coronavirus spread over time. Or, navigate to the 'Data Explorer' tab to view summary tables and graphs",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    animation = TRUE
  )
  
}

# Executes the shiny app
shinyApp(ui = ui, server = server)
