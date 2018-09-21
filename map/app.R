source("../load_packages.R")

load_packages(c(
  "jsonlite",
  "leaflet",
  "leaflet.extras",
  "lubridate",
  "tidyverse"
))

travel <- fromJSON('../Location History.json') %>% 
  .$locations %>% 
  select(timestampMs, latitudeE7, longitudeE7) %>% 
  mutate(time = as.POSIXct(as.numeric(timestampMs)/1000, origin = "1970-01-01 00:00:00")) %>% 
  mutate(lat = latitudeE7/10000000) %>% 
  mutate(lon = longitudeE7/10000000) %>% 
  select(time, lon, lat) %>% 
  mutate(time = round_date(time, "10 mins")) %>% 
  mutate(time = ymd_hms(paste0("2018-01-01 ", hour(.$time), ":", minute(.$time), ":00"),tz = "Asia/Tokyo"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput(inputId = "time", label = "Time", 
                            min = min(travel$time), 
                            max = max(travel$time),
                            value = min(travel$time),
                            step = 60*10,
                            animate = animationOptions(interval = 100, loop = TRUE))
                )
)

server <- function(input, output, session) {
  history <- reactive({
    travel %>%
      filter(time == input$time)
  })
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(139.766084, 35.681382, zoom=13)
  })
  observe({
    leafletProxy("mymap") %>% 
      clearWebGLHeatmap() %>%
      addWebGLHeatmap(lng=~lon, lat=~lat, data = history(), size = 1000, opacity = 0.7)
  })
}

shinyApp(ui, server)