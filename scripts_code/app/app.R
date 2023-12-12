library(shiny)
library(leaflet)
library(tableHTML)
library(sf)
library(crosstalk)
library(tidyverse)
library(leaflet.extras)
library(ggplot2)
library(stringr)
library(rsconnect)
library(plotly)
library(scales)
library(ggiraph)

# load data
load("d.ts.rdata")
load("d.map.rdata")
load("lookup_df.rdata")

# Put United States at top of timeseries area dropdown
sorted_areas <- c("United States", sort(setdiff(d.ts$area, "United States")))


# nested list of groups
nested_choices  <- list("Total population",
                        "White" = list("White",
                                       "White (Hispanic or Latino)",
                                       "White (Not Hispanic or Latino)"),
                        "Black or African American" = list("Black or African American",
                                                           "Black or African American (Hispanic or Latino)",
                                                           "Black or African American (Not Hispanic or Latino)"),
                        "American Indian and Alaska Native" = list("American Indian and Alaska Native",
                                                                   "American Indian and Alaska Native (Hispanic or Latino)",
                                                                   "American Indian and Alaska Native (Not Hispanic or Latino)"),
                        "Asian" = list("Asian",
                                       "Asian (Hispanic or Latino)",
                                       "Asian (Not Hispanic or Latino)",
                                       "Asian Indian",
                                       "Cambodian",
                                       "Chinese",
                                       "Filipino",
                                       "Hmong",
                                       "Japanese",
                                       "Korean",
                                       "Laotian",
                                       "Malaysian",
                                       "Taiwanese",
                                       "Thai",
                                       "Vietnamese"), 
                        "Native Hawaiian and Other Pacific Islander" = list("Native Hawaiian and Other Pacific Islander",
                                                                            "Native Hawaiian and Other Pacific Islander (Hispanic or Latino)",
                                                                            "Native Hawaiian and Other Pacific Islander (Not Hispanic or Latino)",
                                                                            "Native Hawaiian",
                                                                            "Guamanian",
                                                                            "Samoan"),
                        "Hispanic or Latino" = list("Hispanic or Latino (of any race)",
                                                    "Not Hispanic or Latino (of any race)"))

data_choices <- list("Population of group" = "value",
        "Marginal change in group population (%)" = "percent_change",
        "Group's share of total population (%)" = "proportion",
        "Marginal change in group's share of total population (%)" = "proportion_change")


# replace NA in map with user-friendly text
replace_na_with_label <- function(data, replacement = "No Data") {
        ifelse(is.na(data), replacement, data)}


# UI
ui <- fluidPage(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"), # include fontawesome for home button icon
                includeCSS("www/styles.css")),
        #titlePanel("Race and Ethnicity Across Time and States"),
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        fluidRow(id = "linePlotPanel",
                 column(3,
                        selectInput("selectedArea", 
                                    "Area:", 
                                    choices = sorted_areas,
                                    selected = "United States"),
                        selectInput("selectedData", 
                                    "Statistic:", 
                                    choices = data_choices,
                                    selected = "proportion"),
                        selectInput("selectedGroup", 
                                    "Racial/ethnic group(s):", 
                                    choices = nested_choices,
                                    multiple = TRUE,
                                    selected = c("White", 
                                                 "Black or African American", 
                                                 "Asian", 
                                                 "American Indian and Alaska Native",
                                                 "Hispanic or Latino (of any race)"))),
                 column(9,
                        plotlyOutput("linePlot"))),
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MAP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        fluidRow(id = "mapPanel",
                 column(2,
                        selectInput("selectedYear", 
                                    "Year:", 
                                    choices = list(2020, 2010, 2000, 1990, 1980, 1970, 1960, 1950, 1940, 1930, 1920, 1910, 1900, 1890, 1880, 1870,
                                                   1860, 1850, 1840, 1830, 1820, 1810, 1800, 1790),
                                    selected = 2020),
                        selectInput("selectedDataMap", 
                                    "Statistic:", 
                                    choices = data_choices,
                                    selected = "proportion"),
                        selectInput("selectedGroupMap", 
                                    "Racial/ethnic group(s):", 
                                    choices = nested_choices,
                                    selected = "White")),
                 column(6,
                        leafletOutput("choroplethMap")),
                 column(4, girafeOutput("barChart"))))

# Server
server <- function(input, output, session) {
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PLOT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        output$linePlot <- suppressWarnings(renderPlotly({
                subset_data <- subset(d.ts, area == input$selectedArea & group %in% input$selectedGroup)
                
                # Determine the y-axis label based on the input
                y_label <- switch(input$selectedData,
                                  "value" = "Population of group",
                                  "proportion" = "Group's share of total population (%)",
                                  "percent_change" = "Marginal change in group population (%)",
                                  "proportion_change" = "Marginal change in group's \nshare of total population (%)",
                                  "")
                
                # for constricting the y axis, Find the years with any non-NA data for the selected data among the chosen groups
                non_na_years <- unique(subset_data$year[!is.na(subset_data[[input$selectedData]])])
                
                if(length(non_na_years) == 0) {
                        return(ggplot() + 
                                       annotate("text", x = 0.5, y = 0.5, 
                                                label = "No data available for selected group in the selected year.\nPlease refer to the group dropdown menu for available data in a given year.", 
                                                hjust = 0.5, vjust = 0.5) +
                                       theme_void())}
                
                min_year <- min(non_na_years)
                max_year <- max(non_na_years)
                
                p <- ggplot(subset_data, aes(x = year, y = get(input$selectedData), color = group)) +
                        geom_line() +
                        geom_point(aes(text = paste0(group, ": ", if(input$selectedData == "value") {
                                formatC(get(input$selectedData), format = "f", big.mark = ",", digits = 0)
                        } else {
                                formatC(get(input$selectedData), format = "f", big.mark = ",", digits = 1)
                        })), size = 0.1) + 
                        labs(y = y_label,
                                x = "Year",
                                color = "Racial/ethnic group") +
                        theme_minimal() +
                        scale_x_continuous(breaks = unique(subset_data$year), limits = c(min_year, max_year), expand = c(0,0)) +
                        scale_y_continuous(labels = comma) + 
                        theme(axis.text.x = element_text(vjust = 0.5, angle = 45))
                p <- ggplotly(p, tooltip="text") %>% config(displayModeBar = FALSE) 
                
                
                return(p)
        }))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MAP: DROPDOWN MENUS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Modify nested choices based on lookup_df
        update_choices <- function(choices, year) {
                sapply(choices, function(x) {
                        if (is.list(x)) {
                                return(update_choices(x, year))
                        } else {
                                group_name <- x
                                is_disabled <- lookup_df %>% 
                                        filter(year == !!year & group == group_name) %>% 
                                        pull(is_disabled_value) %>% 
                                        { if(length(.) == 0) 1 else .[1] } # default to 1 if group not found
                                
                                if (is_disabled == 1) {
                                        return(paste0(group_name, " -- NO DATA"))
                                } else {
                                        return(group_name)}}})}
        
        # Update selectInput in the server function using reactive context
        observe({
                current_year <- input$selectedYear
                updated_choices <- update_choices(nested_choices, current_year)
                
                # Check if the current selection exists in the updated choices
                current_selection <- input$selectedGroupMap
                
                if(!(current_selection %in% unlist(updated_choices))) {
                        current_selection <- "White"
                }
                
                updateSelectInput(session, "selectedGroupMap", 
                                  choices = updated_choices, 
                                  selected = current_selection)})
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MAP: LEAFLET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        
        output$choroplethMap <- renderLeaflet({
                subset_data <- d.map %>% 
                        filter(year == input$selectedYear, group == gsub(" \\(No data\\)$", "", input$selectedGroupMap))
                
                selected_data <- subset_data[[input$selectedDataMap]]
                
                if (nrow(subset_data) == 0) {
                        leaflet() %>%
                                setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                                addPopups(-98.5, 39.5, "No data available for selected group in the selected year.\nPlease refer to the group dropdown menu for available data in a given year.")
                } else if (all(is.na(selected_data))) {
                        leaflet() %>%
                                setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                                addPopups(-98.5, 39.5, "No data available for selected group in the selected year.\nPlease refer to the group dropdown menu for available data in a given year.")
                } else {
                        
                        # Continuous color scale # picking palette to match ggplot's
                        palette <- colorNumeric(palette = c("#F7FBFF", "#E1EDF8", "#CBDFF1", "#ACD0E6", "#83BADB", "#5AA1CF", "#3987C0", "#1D6AAF", "#084D96", "#08306B"), domain = selected_data)
                        
                        # palette for NAs
                        palette_NA <- colorNumeric(palette = c("#F7FBFF", "#E1EDF8", "#CBDFF1", "#ACD0E6", "#83BADB", "#5AA1CF", "#3987C0", "#1D6AAF", "#084D96", "#08306B"), domain = selected_data, na.color = NA)
                        

                        # ~~~~~~~~~~~~~~~ state label colors (conditionally color based on luminosity of polygon)
                        fillColor <- palette(selected_data)
                        
                        calculate_luminance <- function(color) {
                                # Convert hex color to RGB
                                col <- col2rgb(color) / 255
                                # Luminance formula
                                (0.299 * col[1,] + 0.587 * col[2,] + 0.114 * col[3,])
                        }
                        
                        subset_data$labelColors <- sapply(fillColor, function(color) {
                                luminance <- calculate_luminance(color)
                                ifelse(luminance < 0.5, "white", "black") # Adjust the 0.5 threshold as needed
                        })
                        
                        black_labels_data <- subset(subset_data, labelColors == "black")
                        white_labels_data <- subset(subset_data, labelColors == "white")
                        
                        
                        # Determine the presence of NAs
                        has_na <- any(is.na(selected_data))
                        
                        # Handle labels for NA values
                        data_labels <- ifelse(is.na(selected_data), "No data", formatC(selected_data, format="f", big.mark=",", digits = 1))
                        
                        # Title for the legend
                        selected_data_label <- names(data_choices)[which(data_choices == input$selectedDataMap)]
                        
                        map <- leaflet(
                                options = leafletOptions(maxZoom = 5, minZoom = 3.75, zoomControl = F, zoomSnap = 0.25, zoomDelta = 0.25),
                                data = subset_data) %>%
                                setView(lng = -94.5, lat = 36.5, zoom = 3.75) %>%
                                addPolygons(
                                        fillColor = ~palette(selected_data),
                                        weight = 1,
                                        opacity = 1,
                                        color = "grey",
                                        fillOpacity = 0.7,
                                        highlight = highlightOptions(
                                                weight = 2,
                                                color = "#FFD700",
                                                fillOpacity = 0.7,
                                                bringToFront = TRUE
                                        ),
                                        label = ~ifelse(is.na(selected_data), "No data", if(input$selectedDataMap == "value") {
                                                formatC(selected_data, format = "f", big.mark = ",", digits = 0)
                                        } else {
                                                formatC(selected_data, format = "f", big.mark = ",", digits = 1)
                                        })
                                ) %>%
                                addEasyButton(
                                        easyButton(
                                                icon = "fas fa-home",
                                                title = "Original zoom",
                                                position = "topleft",
                                                onClick = JS(
                                                        paste0(
                                                                "function(btn, map) {
                 map.fitBounds([[", 19, ",", -124, "], [", 50, ",", -67, "]])
               }"
                                                        )
                                                )
                                        )
                                ) %>%
                                addLegend(pal = palette_NA, 
                                          values = selected_data, 
                                          opacity = 1, 
                                          position = "bottomright",
                                          title = paste0(selected_data_label, ": ", input$selectedGroupMap, " (", input$selectedYear, ")"))
                        # add black labels
                                if (nrow(black_labels_data) > 0) {
                                        map <- map %>% addLabelOnlyMarkers(
                                                data = black_labels_data,
                                                lng = ~longitude, lat = ~latitude,
                                                label = ~state_abb,
                                                labelOptions = labelOptions(
                                                        noHide = TRUE, 
                                                        direction = "center", 
                                                        textOnly = TRUE,
                                                        style = list("color" = "black")
                                                )
                                        )
                                }
                        
                        # Add white labels
                        if (nrow(white_labels_data) > 0) {
                                map <- map %>% addLabelOnlyMarkers(
                                        data = white_labels_data,
                                        lng = ~longitude, lat = ~latitude,
                                        label = ~state_abb,
                                        labelOptions = labelOptions(
                                                noHide = TRUE, 
                                                direction = "center", 
                                                textOnly = TRUE,
                                                style = list("color" = "white")
                                        )
                                )
                        }
                                map # return map
                        }})
        
        # Redraw map when the app is resized
        observeEvent(input$mapPanel_resize, {
                leafletProxy("choroplethMap", session) %>% invalidateSize()
        })
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MAP: BAR CHART ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        output$barChart <- renderGirafe({
                # Filter data based on user selection
                subset_data <- d.map %>% 
                        filter(year == input$selectedYear, group == gsub(" \\(No data\\)$", "", input$selectedGroupMap))
                        
                # Creating a sorted dataframe for the bar plot
                subset_data <- subset_data %>%
                        arrange(desc(subset_data))
                
                selected_data_label <- names(data_choices)[which(data_choices == input$selectedDataMap)]
                
                selected_data <- subset_data[[input$selectedDataMap]]
                
                min_val <- min(selected_data, na.rm = TRUE)
                max_val <- max(selected_data, na.rm = TRUE)
                buffer <- (max_val - min_val) * 0.2  # Adjust buffer size as needed

                
                # Check for negative values and adjust ylim
                has_negatives <- any(selected_data < 0, na.rm = TRUE)
                ylim_lower <- if(min_val >= 0) 0 else min_val - .35*buffer
                ylim_upper <- max_val + buffer

                p <- ggplot(subset_data, aes(x = reorder(area, get(input$selectedDataMap)), y = get(input$selectedDataMap), fill = selected_data)) +
                        geom_bar(stat = "identity", color = "grey", linewidth = 0.1) +
                        scale_fill_gradientn(colours = c("#F7FBFF", "#E1EDF8", "#CBDFF1", "#ACD0E6", "#83BADB", "#5AA1CF", "#3987C0", "#1D6AAF", "#084D96", "#08306B")) +
                        geom_text(aes(label = if(input$selectedDataMap == "value") {
                                formatC(selected_data, format = "f", big.mark = ",", digits = 0)
                        } else {
                                formatC(selected_data, format = "f", big.mark = ",", digits = 1)
                        }, hjust = ifelse(selected_data < 0, 1.2, -.2))) +
                        coord_flip(ylim = c(ylim_lower, ylim_upper), expand = F) +
                        labs(x = NULL, y = paste0(selected_data_label, ":", "\n", input$selectedGroupMap, " (", input$selectedYear, ")")) +
                        theme_minimal() + 
                        theme(
                                axis.line.x = element_blank(),    # Remove y-axis line
                                axis.ticks.x = element_blank(),   # Remove y-axis ticks
                                axis.text.x = element_blank(),    # Remove y-axis text 
                                panel.grid = element_blank(),     # Remove all grid lines
                                panel.border = element_blank(), # Remove panel border 
                                axis.text.y = element_text(size = 12),
                                axis.title.x = element_text(size = 14),
                                legend.position = "none")
                girafe(ggobj = p, width_svg = 7, height_svg = 7)
                
        })
        
        }

# Run the app
shinyApp(ui = ui, server = server)
