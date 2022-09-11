# Load libraries, functions, and pre-defied variables
source("functions.R")

OPTIONS <- list_options()

PERIOD_preselected <- OPTIONS |> 
  dplyr::slice_max(period_end, n = 1L) |>
  dplyr::slice(1L) |>
  dplyr::pull(period_menu)

load("poldata/map_districts.RData")

# JavaScript, který se používá pro zjištění velikosti obrazovky uživatele
# jscode <- '
#   $(document).on("shiny:connected", function(e) {
#   var jsHeight = window.innerHeight;
#   Shiny.onInputChange("GetScreenHeight",jsHeight);
#   });
#   '

# Define UI for application that draws a histogram
#### UI ####
ui <- dashboardPage(
  dashboardHeader(
    title = "Analýza dopravních nehod",
    titleWidth = '100%',
    disable = TRUE
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Přehled", tabName = "overview", icon = icon("satellite")),
      menuItem("Nehodové lokality", tabName = "hotspots", icon = icon("map-location-dot")),
      menuItem("Dopravní nehody", tabName = "accidents", icon = icon("car-burst"))#,
      #menuItem("Administrace aplikace", tabName = "administration", icon = icon("tools"))
    ),
    sidebarMenu(
      selectInput(
        "menu_district", "Okres:",
        MENU$district[MENU$district %in% OPTIONS$district],
        selected = "CZ0642",
        multiple = FALSE,
        width = '100%'
      ),
      # selectInput(
      #   "menu_profile",
      #   "Prirotizace nehodových lokalit:",
      #   MENU$profile[MENU$profile %in% OPTIONS$profile],
      #   selected = "default",
      #   width = '100%'
      # ),
      # downloadButton("report", "Stáhnout report"),
      # br(),
      #fillPage(),
      #box(
      img(src='muni-lg-white.png', align = "center", width = "100%"),br(),
      img(src='cuni.png', align = "center", width = "100%"),br(),
      img(src='Doprava.png', align = "center", width = "100%")
      #width = 12,
      #solidHeader = TRUE,
      #background = "navy"
      #)
    ),
    width = 200,
    collapsed = FALSE
  ),
  dashboardBody(
    # fluidRow(
    #   column(7,
    #          box(
    #            h1(textOutput("header_title")),
    #            h4(textOutput("header_period")),
    #            h4(textOutput("header_filteraccidents")),
    #            width = 12
    #          )
    #   ),
    #   column(3,
    #          box(
    #            selectInput(
    #              "menu_profile",
    #              "Prirotizace nehodových lokalit:",
    #              MENU$profile[MENU$profile %in% OPTIONS$profile],
    #              selected = "default",
    #              width = '100%'
    #            ),
    #            width = 12
    #          )
    #   ),
    #   column(2,
    #          box(
    #            downloadButton("generate_report", "PDF")
    #            ),
    #            width = 12
    #          )
    # ),
    ##### UI: Overview #####
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                column(6,
                       box(
                         h1(textOutput("header_title")),
                         h4(textOutput("header_period")),
                         h4(textOutput("header_filteraccidents")),
                         downloadButton("report_overview", "Report"),
                         width = 12
                       )
                ),
                column(3,
                       box(
                         dateRangeInput(
                           "menu_period",
                           "Období (od/do):",
                           #unique(OPTIONS$period_menu),
                           #selected = PERIOD_preselected,
                           start = max(OPTIONS$period_start),
                           end = max(OPTIONS$period_end),
                           min = "2011-01-01",
                           max = max(OPTIONS$period_end),
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = " do ",
                           width = '100%'
                         ),
                         width = 12
                       )
                ),
                column(3,
                       box(
                         selectInput(
                           "menu_filteraccidents",
                           "Výběr dopravních nehod:",
                           MENU$filteraccidents,
                           selected = "all",
                           width = '100%'
                         ),
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(3,
                       box(
                         valueBoxOutput("box_acc_n", width = 12),
                         valueBoxOutput("box_acc_n_delta", width = 12),
                         title = "Dopravních nehod",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         valueBoxOutput("box_death_n", width = 12),
                         valueBoxOutput("box_death_n_delta", width = 12),
                         title = "Usmrceno osob",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         valueBoxOutput("box_swound_n", width = 12),
                         valueBoxOutput("box_swound_n_delta", width = 12),
                         title = "Těžce zraněno osob",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         valueBoxOutput("box_lwound_n", width = 12),
                         valueBoxOutput("box_lwound_n_delta", width = 12),
                         title = "Lehce zraněno osob",
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_casualties"),
                         title = "Oběti dopravních nehod",
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_faults"),
                         title = "Zavinění dopravních nehod",
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_causes"),
                         title = "Nejčastější příčiny dopravních nehod",
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_mosttragic"),
                         title = "Nejtragičtější příčiny dopravních nehod",
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_crashtype"),
                         title = "Druh srážky",
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_obstacle"),
                         title = "Srážka s pevnou překážkou",
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_age"),
                         title = "Počet nehod podle věku řidiče",
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_agedead"),
                         title = "Počet zemřelých podle věku řidiče",
                         width = 12
                       )
                )
              )
      ),
      ##### UI: Hotspots #####
      tabItem(tabName = "hotspots",
              fluidRow(
                column(3,
                       box(
                         selectInput(
                           "menu_period_hotspots",
                           "Období:",
                           unique(OPTIONS$period_menu),
                           selected = PERIOD_preselected,
                           width = '100%'
                         ),
                         width = 12
                       )
                ),
                column(3,
                       box(
                         selectInput(
                           "menu_profile",
                           "Prirotizace nehodových lokalit:",
                           MENU$profile[MENU$profile %in% OPTIONS$profile],
                           selected = "default",
                           width = '100%'
                         ),
                         width = 12
                       )
                ),
                column(3,
                       box(
                         sliderInput(
                           "menu_parameterA",
                           "Parametr pro výpočet:",
                           min = 0,
                           max = 100,
                           value = 50,
                           width = '100%'
                         ),
                         width = 12
                       )
                ),
                column(3,
                       box(
                         sliderInput(
                           "menu_parameterB",
                           "Parametr pro výpočet:",
                           min = 0,
                           max = 100,
                           value = 50,
                           width = '100%'
                         ),
                         width = 12
                       )
                )
              ),
      fluidRow(
        column(6,
               box(
               radioGroupButtons(
                 "menu_hotspot",
                 "Výbě hotspotu",
                 choices = 1:10,
                 selected = 1,
                 justified = TRUE
               ),
               leafletOutput("mphot", height = 900),
               width = 12
               )
               )
        )
      ),
      ##### UI: Accidents #####
      tabItem(tabName = "accidents",
              tags$script("
                          Shiny.addCustomMessageHandler('map_selection', function(value) {
                          Shiny.setInputValue('map_selection', value);
                          });
                          "),
              fluidRow(
                column(4,
                       box(
                         dateRangeInput(
                           "menu_period_accidents",
                           "Období (od/do):",
                           #unique(OPTIONS$period_menu),
                           #selected = PERIOD_preselected,
                           start = max(OPTIONS$period_start),
                           end = max(OPTIONS$period_end),
                           min = "2011-01-01",
                           max = max(OPTIONS$period_end),
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = " do ",
                           width = '100%'
                         ),
                         width = 12
                       )
                ),
                column(3,
                       box(
                         selectInput(
                           "menu_filteraccidents_accidents",
                           "Výběr dopravních nehod:",
                           MENU$filteraccidents,
                           selected = "all",
                           width = '100%'
                         ),
                         width = 12
                       )
                ),
                column(5,
                       box(
                         p("Výběr polygonu na mapě omezuje výběr dopravních nehod na danou oblast. 
                              Ostatní nastavené filtry zůstavájí v platnosti (AND)."),
                         actionButton("removefilter", "Filtr není aktivní"),
                         #title = "Výběr oblasti",
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(7,
                       box(
                            leafletOutput("mpacc", height = 900),
                            title = "Mapa dopravních nehod",
                            width = 12
                          ),
                      box(
                            radioButtons("mpacc_legend", label = "",
                            choices = list(
                              "Dopravní nehody podle následků" = "nasledky", 
                              "Druh nehody" = "druh_nehody"
                            ), 
                            selected = "nasledky"),
                            width = 6
                            )
                        ),
              column(5,
                      # box(
                      #      p("Výběr polygonu na mapě omezuje výběr dopravních nehod na danou oblast. 
                      #         Ostatní nastavené filtry zůstavájí v platnosti (AND)."),
                      #      actionButton("removefilter", "Filtr není aktivní"),
                      #      title = "Výběr oblasti",
                      #      width = 12
                      #   ),
                      # box(
                      #     p("Filtr umožňuje detailní nastavení období. Pokud není nastaven, potom
                      #       se použijí všechny nehody z nastaveného období. Tento filtr platí pouze
                      #       pro analýzu jednotlivých dopravních nehod."),
                      #     materialSwitch(
                      #       "periodfilter",
                      #       "Zapnout detailní filtrování období"
                      #       ),
                      #     uiOutput("periodfilterinput"),
                      #     title = "Výběr časového období (nepovinný)",
                      #     width = 7
                      #   ),
                        valueBoxOutput("box_acc_n_box", width = 3),
                        valueBoxOutput("box_death_n_box", width = 3),
                        valueBoxOutput("box_swound_n_box", width = 3),
                        valueBoxOutput("box_lwound_n_box", width = 3),
                     box(
                       tableOutput("tab_casualties_accidents"), 
                       title = "Oběti dopravních nehod",
                       width = 12
                     ),
                     box(
                       tableOutput("tab_fault_accidents"), #tab_causes_accidents
                       title = "Nehody podle zavinění",
                       width = 12
                     ),
                     box(
                       tableOutput("tab_causes_accidents"), #tab_type_accidents
                       title = "Nehody podle příčin",
                       width = 12
                     ),
                     box(
                       tableOutput("tab_type_accidents"), #tab_type_accidents
                       title = "Nehody podle příčin",
                       width = 12
                     ),
                     box(
                       plotOutput("fig_age_box"),
                       title = "Počet nehod podle věku řidiče",
                       width = 12
                     )
              )
          ) # Fluid row ends here
      )
    )
  ),
  title = "Analýza dopravních nehod",
  skin = "black"
)

#### Server ####
server <- function(input, output, session) {
  
  ##### Header #####
  
  output$header_title <- renderText({
    stringr::str_c("Dopravní nehody v okrese ",
                   names(MENU$district)[MENU$district == input$menu_district])
  })
  
  output$header_period <- renderText({
    # Duration of p1 in days
    p2_length <- input$menu_period[2] - input$menu_period[1]
    p2_length <- as.double(p2_length)
    
    # End date of comparison period (p2)
    p2_end <- input$menu_period[1]
    p2_end <- p2_end - 1
    
    # Start date of comparison period (p2)
    p2_start <- p2_end - p2_length
    
    p2_string <- stringr::str_c(
      strftime(p2_start, format = "%d.%m.%Y"),
      " - ",
      strftime(p2_end, format = "%d.%m.%Y")
    )
    
    stringr::str_c("Základní období: ",
                   strftime(input$menu_period[1], format = "%d.%m.%Y"),
                   " - ",
                   strftime(input$menu_period[2], format = "%d.%m.%Y"),
                   "; Srovnávací období: ",
                   p2_string
    )
  })
  
  output$header_filteraccidents <- renderText({
    stringr::str_c("Dopravní nehody ve výběru: ",
                   names(MENU$filteraccidents)[MENU$filteraccidents == input$menu_filteraccidents])
  })
  
  ##### Get data #####
  
  # Returns car accident from user-defined period
  get_accidents <- reactive({
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(
        accident_date >= input$menu_period_accidents[1]
      ) |>
      dplyr::filter(
        accident_date <= input$menu_period_accidents[2]
      ) 
    
    if(input$menu_filteraccidents_accidents == "all"){
      return(out)
    }else{
      
      if(input$menu_filteraccidents_accidents == "pedestrian"){
        return(dplyr::filter(out, involved_pedestrian))
      }
      
      if(input$menu_filteraccidents_accidents == "bike"){
        return(dplyr::filter(out, involved_bike))
      }
      
      if(input$menu_filteraccidents_accidents == "motobike"){
        return(dplyr::filter(out, involved_motobike))
      }
      
      if(input$menu_filteraccidents_accidents == "alcohol"){
        return(dplyr::filter(out, !accident_alcohol %in% c(0,1)))
      }
    }
    
  })
  
  # Returns car accident from user-defined period
  get_accidents_box <- reactive({
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(
        accident_date >= input$menu_period_accidents[1]
      ) |>
      dplyr::filter(
        accident_date <= input$menu_period_accidents[2]
      )
    
    if(length(input$map_selection) != 0){
      if(input$map_selection[1] != "none"){
      out <-
        out |>
          dplyr::filter(
            accident_id %in% input$map_selection
          )
      }
    }
    
    if(input$menu_filteraccidents_accidents == "all"){
      return(out)
    }else{
      
      if(input$menu_filteraccidents_accidents == "pedestrian"){
        return(dplyr::filter(out, involved_pedestrian))
      }
      
      if(input$menu_filteraccidents_accidents == "bike"){
        return(dplyr::filter(out, involved_bike))
      }
      
      if(input$menu_filteraccidents_accidents == "motobike"){
        return(dplyr::filter(out, involved_motobike))
      }
      
      if(input$menu_filteraccidents_accidents == "alcohol"){
        return(dplyr::filter(out, !accident_alcohol %in% c(0,1)))
      }
    }
    
  })
  
  # Returns car accident from the baseline period
  get_accidents_p1 <- reactive({
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(
        accident_date >= input$menu_period[1]
      ) |>
      dplyr::filter(
        accident_date <= input$menu_period[2]
      ) 
    
    if(input$menu_filteraccidents == "all"){
      return(out)
    }else{
      
      if(input$menu_filteraccidents == "pedestrian"){
        return(dplyr::filter(out, involved_pedestrian))
      }
      
      if(input$menu_filteraccidents == "bike"){
        return(dplyr::filter(out, involved_bike))
      }
      
      if(input$menu_filteraccidents == "motobike"){
        return(dplyr::filter(out, involved_motobike))
      }
      
      if(input$menu_filteraccidents == "alcohol"){
        return(dplyr::filter(out, !accident_alcohol %in% c(0,1)))
      }
    }
    
  })
  
  # Returns car accidents from comparison period
  get_accidents_p2 <- reactive({
    
    # Duration of p1 in days
    p2_length <- input$menu_period[2] - input$menu_period[1]
    p2_length <- as.double(p2_length)
    
    # End date of comparison period (p2)
    p2_end <- input$menu_period[1]
    p2_end <- p2_end - 1
    
    # Start date of comparison period (p2)
    p2_start <- p2_end - p2_length
    
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(
        accident_date >= p2_start
      ) |>
      dplyr::filter(
        accident_date <= p2_end
      ) 
    
    if(input$menu_filteraccidents == "all"){
      return(out)
    }else{
      
      if(input$menu_filteraccidents == "pedestrian"){
        return(dplyr::filter(out, involved_pedestrian))
      }
      
      if(input$menu_filteraccidents == "bike"){
        return(dplyr::filter(out, involved_bike))
      }
      
      if(input$menu_filteraccidents == "motobike"){
        return(dplyr::filter(out, involved_motobike))
      }
      
      if(input$menu_filteraccidents == "alcohol"){
        return(dplyr::filter(out, !accident_alcohol %in% c(0,1)))
      }
    }
    
  })
  
  
  ##### Overview #####
  
  ###### Boxes with stats on accidents #####
  
  # Number of accidents
  output$box_acc_n <- renderValueBox({
    n1 <- nrow(get_accidents_p1())
    
    pr <- format(n1, trim = TRUE, nsmall = 0L, big.mark = " ")
    
    valueBox(
      pr, "Ve sledovaném období", 
      color = "light-blue",
      icon = icon("car-burst")
    )
  })
  
  output$box_acc_n_delta <- renderValueBox({
    n1 <- nrow(get_accidents_p1())
    n2 <- nrow(get_accidents_p2())
    
    pr <- get_delta(n1,n2)
    
    valueBox(
      pr, "Změna proti srovnávacímu období", 
      color = ifelse((n1-n2)>0,"red","green"),
      icon = icon(get_arrow(n1-n2))
    )
  })
  
  # Number of accidents with casualties
  
  output$box_death_n <- renderValueBox({
    n1 <- get_accidents_p1() |> 
      dplyr::pull(accident_dead) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = TRUE, nsmall = 0L, big.mark = " ")
    
    valueBox(
      pr, "Ve sledovaném období", 
      color = "light-blue",
      icon = icon("skull-crossbones")
    )
  })
  
  output$box_death_n_delta <- renderValueBox({
    n1 <- get_accidents_p1() |> 
      dplyr::pull(accident_dead) |> 
      sum(na.rm = TRUE)
    
    n2 <- get_accidents_p2() |> 
      dplyr::pull(accident_dead) |> 
      sum(na.rm = TRUE)
    
    pr <- get_delta(n1,n2)
    
    valueBox(
      pr, "Změna proti srovnávacímu období", 
      color = ifelse((n1-n2)>0,"red","green"),
      icon = icon(get_arrow(n1-n2))
    )
  })
  
  # Number of accidents with serious injuries
  
  output$box_swound_n <- renderValueBox({
    n1 <- get_accidents_p1() |> 
      dplyr::pull(accident_serious_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = TRUE, nsmall = 0L, big.mark = " ")
    
    valueBox(
      pr, "Ve sledovaném období", 
      color = "light-blue",
      icon = icon("crutch")
    )
  })
  
  output$box_swound_n_delta <- renderValueBox({
    n1 <- get_accidents_p1() |> 
      dplyr::pull(accident_serious_injury) |> 
      sum(na.rm = TRUE)
    
    n2 <- get_accidents_p2() |> 
      dplyr::pull(accident_serious_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- get_delta(n1,n2)
    
    valueBox(
      pr, "Změna proti srovnávacímu období", 
      color = ifelse((n1-n2)>0,"red","green"),
      icon = icon(get_arrow(n1-n2))
    )
  })
  
  # Number of accidents with light injuries
  
  output$box_lwound_n <- renderValueBox({
    n1 <- get_accidents_p1() |> 
      dplyr::pull(accident_light_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = TRUE, nsmall = 0L, big.mark = " ")
    
    valueBox(
      pr, "Ve sledovaném období", 
      color = "light-blue",
      icon = icon("user-injured")
    )
  })
  
  output$box_lwound_n_delta <- renderValueBox({
    n1 <- get_accidents_p1() |> 
      dplyr::pull(accident_light_injury) |> 
      sum(na.rm = TRUE)
    
    n2 <- get_accidents_p2() |> 
      dplyr::pull(accident_light_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- get_delta(n1,n2)
    
    valueBox(
      pr, "Změna proti srovnávacímu období", 
      color = ifelse((n1-n2)>0,"red","green"),
      icon = icon(get_arrow(n1-n2))
    )
  })
  
  ###### Figures #####
  
  output$fig_casualties <- renderPlot({
    p1 <- get_accidents_p1() |>
      sf::st_drop_geometry() |> 
      dplyr::select(
        starts_with("casualties")
      ) |> 
      dplyr::summarise(
        across(
          everything(),
          sum
        )
      ) |> 
      tidyr::pivot_longer(
        everything()
      )
    
    p2 <- get_accidents_p2() |>
      sf::st_drop_geometry() |> 
      dplyr::select(
        starts_with("casualties")
      ) |>
      dplyr::summarise(
        across(
          everything(),
          sum
        )
      ) |> 
      tidyr::pivot_longer(
        everything()
      )
    
    dplyr::bind_rows(
      dplyr::mutate(p1, period = "p1"),
      dplyr::mutate(p2, period = "p2")
    ) |>
      dplyr::arrange(period,value) %>% 
      dplyr::mutate(
        name = factor(name, levels = unique(name)),
        period = factor(period, levels = c("p2","p1"))
      ) |>
      ggplot(
        aes(y = name, x = value, fill = period)
      ) +
      geom_col(
        position = "dodge"
      ) +
      scale_x_continuous(
        "Počet obětí"
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = "Základní období",
          "p2" = "Srovnávací období"
        )
      ) +
      scale_y_discrete(
        labels = casualties
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_faults <- renderPlot({
    p1 <- get_accidents_p1() |>
      sf::st_drop_geometry() |>
      dplyr::group_by(
        accident_fault
      ) |> 
      dplyr::summarise(
        value = dplyr::n()
      )
    
    p2 <- get_accidents_p2() |>
      sf::st_drop_geometry() |> 
      dplyr::group_by(
        accident_fault
      ) %>% 
      dplyr::summarise(
        value = dplyr::n()
      )
    
    dplyr::bind_rows(
      dplyr::mutate(p1, period = "p1"),
      dplyr::mutate(p2, period = "p2")
    ) %>% 
      dplyr::arrange(period,value) |> 
      dplyr::mutate(
        accident_fault = factor(accident_fault, levels = unique(accident_fault)),
        period = factor(period, levels = c("p2","p1"))
      ) |>
      ggplot(
        aes(y = accident_fault, x = value, fill = period)
      ) +
      geom_col(
        position = "dodge"
      ) +
      scale_x_continuous(
        "Počet nehod"
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = "Základní období",
          "p2" = "Srovnávací období"
        )
      ) +
      scale_y_discrete(
        labels = faults
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_causes <- renderPlot({
    get_accidents_p1() |>
      sf::st_drop_geometry() |>
      dplyr::select(
        accident_cause
      ) |>
      dplyr::group_by(
        accident_cause
      ) |>
      dplyr::summarise(
        value = dplyr::n()
      ) |>
      dplyr::slice_max(value, n = 5) |>
      dplyr::arrange(value) |>
      mutate(
        accident_cause = factor(accident_cause, levels = accident_cause)
      ) |>
      ggplot(
        aes(x = value, y = accident_cause)
      ) +
      geom_col(
        fill = "#377eb8"
      ) +
      scale_x_continuous(
        "Počet nehod"
      ) +
      scale_y_discrete(
        labels = causes
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_mosttragic <- renderPlot({
    get_accidents_p1() |>
      sf::st_drop_geometry() |>
      dplyr::filter(accident_dead > 0) |>
      dplyr::select(
        accident_cause,accident_dead
      ) |>
      dplyr::group_by(
        accident_cause
      ) |>
      dplyr::summarise(
        value = as.integer(sum(accident_dead, na.rm = TRUE))
      ) |>
      dplyr::slice_max(value, n = 5) |>
      dplyr::arrange(value) |>
      mutate(
        accident_cause = factor(accident_cause, levels = accident_cause)
      ) |>
      ggplot(
        aes(x = value, y = accident_cause)
      ) +
      geom_col(
        fill = "#377eb8"
      ) +
      scale_x_continuous(
        "Počet zemřelých"
      ) +
      scale_y_discrete(
        labels = causes
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_crashtype <- renderPlot({
    p1 <- get_accidents_p1() |>
      sf::st_drop_geometry() |>
      dplyr::filter(accident_type != 0) |>
      dplyr::group_by(
        accident_type
      ) |> 
      dplyr::summarise(
        value = dplyr::n()
      )
    
    p2 <- get_accidents_p2() |>
      sf::st_drop_geometry() |> 
      dplyr::filter(accident_type != 0) |>
      dplyr::group_by(
        accident_type
      ) %>% 
      dplyr::summarise(
        value = dplyr::n()
      )
    
    dplyr::bind_rows(
      dplyr::mutate(p1, period = "p1"),
      dplyr::mutate(p2, period = "p2")
    ) %>% 
      tidyr::complete(period,accident_type,fill = list(value = 0)) |>
      dplyr::arrange(period,value) |> 
      dplyr::mutate(
        accident_type = factor(accident_type, levels = unique(accident_type)),
        period = factor(period, levels = c("p2","p1"))
      ) |>
      ggplot(
        aes(y = accident_type, x = value, fill = period)
      ) +
      geom_col(
        position = "dodge"
      ) +
      scale_x_continuous(
        "Počet nehod"
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = "Základní období",
          "p2" = "Srovnávací období"
        )
      ) +
      scale_y_discrete(
        labels = crashtype
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_obstacle <- renderPlot({
    p1 <- get_accidents_p1() |>
      sf::st_drop_geometry() |>
      dplyr::filter(accident_obstacle_type != 0) |>
      dplyr::group_by(
        accident_obstacle_type
      ) |> 
      dplyr::summarise(
        value = dplyr::n()
      )
    
    p2 <- get_accidents_p2() |>
      sf::st_drop_geometry() |> 
      dplyr::filter(accident_obstacle_type != 0) |>
      dplyr::group_by(
        accident_obstacle_type
      ) |>
      dplyr::summarise(
        value = dplyr::n()
      )
    
    dplyr::bind_rows(
      dplyr::mutate(p1, period = "p1"),
      dplyr::mutate(p2, period = "p2")
    ) %>% 
      tidyr::complete(period,accident_obstacle_type,fill = list(value = 0)) |>
      dplyr::arrange(period,value) |> 
      dplyr::mutate(
        accident_obstacle_type = factor(accident_obstacle_type, levels = unique(accident_obstacle_type)),
        period = factor(period, levels = c("p2","p1"))
      ) |>
      ggplot(
        aes(y = accident_obstacle_type, x = value, fill = period)
      ) +
      geom_col(
        position = "dodge"
      ) +
      scale_x_continuous(
        "Počet nehod"
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = "Základní období",
          "p2" = "Srovnávací období"
        )
      ) +
      scale_y_discrete(
        labels = obstacletype
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_age <- renderPlot({
    get_accidents_p1() |>
      sf::st_drop_geometry() |>
      tidyr::drop_na(driver_age) |>
      dplyr::mutate(
        agecat = cut(driver_age, breaks = c(0,5,9,14,17,20,24,34,44,54,64,Inf))
      ) |>
      dplyr::group_by(
        agecat,
        .drop = FALSE
      ) |> 
      dplyr::summarise(
        value = dplyr::n()
      ) |>
      ggplot(
        aes(y = value, x = agecat)
      ) +
      geom_col(
        fill = "#377eb8"
      ) +
      scale_x_discrete(
        "Věk řidiče"
      ) +
      scale_y_continuous(
        "Počet nehod"
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$fig_agedead <- renderPlot({
    get_accidents_p1() |>
      sf::st_drop_geometry() |>
      tidyr::drop_na(driver_age) |>
      dplyr::mutate(
        agecat = cut(driver_age, breaks = c(0,5,9,14,17,20,24,34,44,54,64,Inf))
      ) |>
      dplyr::group_by(
        agecat
      ) |> 
      dplyr::summarise(
        value = sum(accident_dead)
      ) |>
      ggplot(
        aes(y = value, x = agecat)
      ) +
      geom_col(
        fill = "#377eb8"
      ) +
      scale_x_discrete(
        "Věk řidiče"
      ) +
      scale_y_continuous(
        "Počet zemřelých"
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  
  ##### Hotspots #####
  
  ###### Read hotspots #####
  get_hotspots <- reactive({
    
    return(read_hotspots(
      district = input$menu_district,
      profile = input$menu_profile,
      period_start = first(OPTIONS$period_start[OPTIONS$period_menu == input$menu_period_hotspots]),
      period_end = first(OPTIONS$period_end[OPTIONS$period_menu == input$menu_period_hotspots])
    ))
    
  })
  
  ###### Figures ######
  
  output$fig_clusters <- renderPlot({
    data_hotspots <- get_hotspots()
    
    data_hotspots$cluster_statistics |>
      ggplot(
        aes(x = factor(cluster), y = cost)
      ) +
      geom_col()
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  ###### Maps ######
  
  output$mphot <- renderLeaflet({
    
    data_hotspots <- get_hotspots()
    
    hotspots_accidents <- get_accidents() |>
      dplyr::filter(
        accident_id %in% data_hotspots$accidents$accident_id
      )
    
    hotspots <- data_hotspots$cluster_statistics |>
      dplyr::select(
        cluster
      )
    
    hotspots_point <- 
      data_hotspots$cluster_statistics |>
      st_centroid() |>
      dplyr::select(
        cluster
      )
    
    leaflet(data = get_map_district()) |>
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |>
      addTiles(group = "OSM (default)") |>
      addProviderTiles('Esri.WorldImagery', group = "Satelite") |>
      addPolygons(
        fill = FALSE
      ) |> 
      # addPolylines(
      #   data = densities,
      #   group = "Hustoty",
      #   color = "#41b6c4"
      # ) |>
      addPolylines(
        data = sf::st_geometry(hotspots),
        group = "Hustoty",
        color = "red"
      ) |>
      addCircleMarkers(
        data = st_jitter(
          hotspots_accidents, factor = 0.0002
        ),
        radius = 5,
        color = "black",
        weight = 1,
        fillOpacity = 0.5
      ) |>
      addLabelOnlyMarkers(
        data = hotspots_point,
        #lng = ~X,
        #lat = ~Y,
        label = ~as.character(cluster),
        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = "20px")
      ) |>
      addLayersControl(
        baseGroups = c("Positron", "OSM (default)", "Satelite"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomright"
      ) |>
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers"
      )
    
  })
  
  
  
  ##### Accidents #####
  
  ###### Boxes #####
  
  output$box_acc_n_box <- renderValueBox({
    n1 <- nrow(get_accidents_box())
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, "Dopravních nehod", 
      color = "light-blue",
      icon = icon("car-burst")
    )
  })
  
  output$box_death_n_box <- renderValueBox({
    n1 <- get_accidents_box() |> 
      dplyr::pull(accident_dead) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, "Usmrceno osob", 
      color = "light-blue",
      icon = icon("skull-crossbones")
    )
  })
  
  output$box_swound_n_box <- renderValueBox({
    n1 <- get_accidents_box() |> 
      dplyr::pull(accident_serious_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, "Těžce zraněno osob", 
      color = "light-blue",
      icon = icon("crutch")
    )
  })
  
  output$box_lwound_n_box <- renderValueBox({
    n1 <- get_accidents_box() |> 
      dplyr::pull(accident_light_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, "Lehce zraněno osob", 
      color = "light-blue",
      icon = icon("user-injured")
    )
  })
  
  ###### Tables/figures ######
  
  output$tab_casualties_accidents <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
    out <- get_accidents_box() |>
      sf::st_drop_geometry() |> 
      dplyr::select(
        starts_with("casualties")
      ) |> 
      dplyr::summarise(
        across(
          everything(),
          sum
        )
      ) |> 
      tidyr::pivot_longer(
        everything()
      ) |>
      dplyr::filter(value > 0) |>
      dplyr::mutate(
        name = factor(
          name, 
          levels = names(casualties_nolinebreaks),
          labels = casualties_nolinebreaks
        ) |> as.character()
      ) |>
      dplyr::arrange(desc(value)) %>%
      dplyr::mutate(
        name = factor(name, levels = unique(name)),
        value = format(value, trim = TRUE, nsmall = 0, big.mark = " ")
      )
      
      if(nrow(out) != 0){
        return(out)
      }else{
        return(tribble(
          ~name, ~value,
          "Ve výběru nejsou žádné smrtelné nehody.",""
        ))
      }
  })
  
  output$tab_fault_accidents <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      out <- get_accidents_box() |>
        sf::st_drop_geometry() |> 
        dplyr::group_by(accident_fault) |>
        dplyr::summarise(
          value = n(),
          .groups = "drop"
        ) |>
        dplyr::rename(name = accident_fault) |>
        dplyr::filter(value > 0) |>
        dplyr::mutate(
          name = factor(
            name, 
            levels = names(faults_nolinebreaks),
            labels = faults_nolinebreaks
          ) |> as.character()
        ) |>
        dplyr::arrange(desc(value)) %>%
        dplyr::mutate(
          name = factor(name, levels = unique(name)),
          value = format(value, trim = TRUE, nsmall = 0, big.mark = " ")
        )
      
      if(nrow(out) != 0){
        return(out)
      }else{
        return(tribble(
          ~name, ~value,
          "Ve výběru nejsou žádné zaviněné nehody.",""
        ))
      }
    })
  
  
  output$tab_causes_accidents <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      out <- get_accidents_box() |>
        sf::st_drop_geometry() |>
        dplyr::select(
          accident_cause
        ) |>
        dplyr::group_by(
          accident_cause
        ) |>
        dplyr::summarise(
          value = dplyr::n()
        ) |>
        dplyr::slice_max(value, n = 10) |>
        dplyr::mutate(
          accident_cause = factor(accident_cause, 
                                  levels = names(causes_nolinebreaks),
                                  labels = causes_nolinebreaks
                                  ) |> as.character()
        ) |>
        dplyr::arrange(desc(value)) |>
        dplyr::mutate(
          accident_cause = factor(accident_cause, 
                                  levels = accident_cause),
          value = format(value, trim = TRUE, nsmall = 0, big.mark = " ")
        ) |>
        dplyr::rename(name = accident_cause)
      
      if(nrow(out) != 0){
        return(out)
      }else{
        return(tribble(
          ~name, ~value,
          "Ve výběru nejsou žádné nehody se známou příčinou.",""
        ))
      }
    })
  
  output$tab_type_accidents <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      out <- get_accidents_box() |>
        sf::st_drop_geometry() |>
        dplyr::select(
          accident_type
        ) |>
        dplyr::filter(
          accident_type != 0
        ) |>
        dplyr::group_by(
          accident_type
        ) |>
        dplyr::summarise(
          value = dplyr::n()
        ) |>
        dplyr::slice_max(value, n = 5) |>
        dplyr::mutate(
          accident_type = factor(accident_type, 
                                  levels = names(crashtype_nolinebreaks),
                                  labels = crashtype_nolinebreaks
          ) |> as.character()
        ) |>
        dplyr::arrange(desc(value)) |>
        dplyr::mutate(
          accident_type = factor(accident_type, 
                                  levels = accident_type),
          value = format(value, trim = TRUE, nsmall = 0, big.mark = " ")
        ) |>
        dplyr::rename(name = accident_type)
      
      if(nrow(out) != 0){
        return(out)
      }else{
        return(tribble(
          ~name, ~value,
          "Ve výběru nejsou žádné nehody se známou příčinou.",""
        ))
      }
    })
  
  output$fig_age_box <- renderPlot({
    get_accidents_box() |>
      sf::st_drop_geometry() |>
      tidyr::drop_na(driver_age) |>
      dplyr::mutate(
        agecat = cut(driver_age, breaks = c(0,5,9,14,17,20,24,34,44,54,64,Inf))
      ) |>
      dplyr::group_by(
        agecat,
        .drop = FALSE
      ) |> 
      dplyr::summarise(
        value = dplyr::n()
      ) |>
      ggplot(
        aes(y = value, x = agecat)
      ) +
      geom_col(
        fill = "#377eb8"
      ) +
      scale_x_discrete(
        "Věk řidiče"
      ) +
      scale_y_continuous(
        "Počet nehod"
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  ###### Maps #####
  get_map_district <- reactive({
    map_districts |>
      filter(KOD_OKRES == input$menu_district) |>
      st_geometry()
  })
  
  output$mpacc <- renderLeaflet({
    
    fdata <- get_accidents() |>
      dplyr::mutate(
        nasledky = dplyr::case_when(
          accident_dead > 0 ~ "Nehoda s obětí na životech",
          accident_serious_injury > 0 ~ "Nehoda s těžkým zraněním",
          accident_light_injury > 0 ~ "Nehoda s lehkým zraněním",
          TRUE ~ "Ostatní nehody"
        )
      )
    
    fpal <- colorFactor("Set1", domain = fdata$nasledky, levels = levels(fdata$nasledky))
    
    leaflet(data = get_map_district()) |>
      #leaflet(data = select_()) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |>
      addTiles(group = "OSM (default)") |>
      addProviderTiles('Esri.WorldImagery', group = "Satelite") |>
      addPolygons(
        fill = FALSE
      ) %>% 
      addLayersControl(
        baseGroups = c("Positron", "OSM (default)", "Satelite"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomright"
      )%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers"
      ) %>%
      addDrawToolbar(
        targetGroup="draw",
        position = "bottomleft",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(),
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = FALSE,
        singleFeature = TRUE
      )  |> 
      addCircleMarkers(
        data = st_jitter(
          fdata, factor = 0.0002
        ),
        #lng = ~X,
        #lat = ~Y,
        radius = 5,
        color = "black",
        weight = 1,
        fillColor = ~fpal(nasledky),
        fillOpacity = 0.5
      ) %>% 
      addLegend(
        title = "Následky nehody",
        position = "topright",
        pal = fpal,
        values = fdata$nasledky, 
        opacity = 1
      )
  })
  
  observe(if(input$mpacc_legend=="nasledky"){
    
    fdata <- get_accidents() |>
      dplyr::mutate(
        nasledky = dplyr::case_when(
          accident_dead > 0 ~ "Nehoda s obětí na životech",
          accident_serious_injury > 0 ~ "Nehoda s těžkým zraněním",
          accident_light_injury > 0 ~ "Nehoda s lehkým zraněním",
          TRUE ~ "Ostatní nehody"
        )
      )
    
    fpal <- colorFactor("Set1", domain = fdata$nasledky, levels = levels(fdata$nasledky))
    
    leafletProxy("mpacc") |>
      clearControls() |>
      clearMarkers() |> 
      addCircleMarkers(
        data = st_jitter(
          fdata, factor = 0.0002
        ),
        #lng = ~X,
        #lat = ~Y,
        radius = 5,
        color = "black",
        weight = 1,
        fillColor = ~fpal(nasledky),
        fillOpacity = 0.5
      ) %>% 
      addLegend(
        title = "Následky nehody",
        position = "topright",
        pal = fpal,
        values = fdata$nasledky, 
        opacity = 1
      )
    
  })
  
  observe(if(input$mpacc_legend=="druh_nehody"){
    
    fdata <- get_accidents() |>
      dplyr::mutate(
        druh_nehody = factor(accident_type, 
                             levels = names(crashtype_nolinebreaks),
                             labels = crashtype_nolinebreaks
                             ),
        druh_nehody = as.character(druh_nehody)
      )
      
    fpal <- colorFactor("Set1", domain = fdata$druh_nehody, levels = levels(fdata$druh_nehody))
    
    leafletProxy("mpacc") |>
      clearControls() |>
      clearMarkers() |> 
      addCircleMarkers(
        data = st_jitter(
          fdata, factor = 0.0002
        ),
        #lng = ~X,
        #lat = ~Y,
        radius = 5,
        color = "black",
        weight = 1,
        fillColor = ~fpal(druh_nehody),
        fillOpacity = 0.5
      ) %>% 
      addLegend(
        title = "Druh nehody",
        position = "topright",
        pal = fpal,
        values = fdata$druh_nehody, 
        opacity = 1
      )
    
  })
  
  #### Výběr polygonu na mapě
  
  # https://redoakstrategic.com/geoshaper/
  observeEvent(input$mpacc_draw_new_feature,{
    polygon_coordinates <- input$mpacc_draw_new_feature$geometry$coordinates[[1]]
    
    # Create sf polygon
    map_selection_polygon <- 
      do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})) |>
      sf::st_multipoint() |>
      sf::st_cast("POLYGON") |>
      sf::st_sfc() |>
      sf::st_set_crs(4326)
    
    map_selection_vec <- 
      map_selection_polygon |>
      sf::st_intersection(get_accidents(),y = _) |>
      dplyr::pull(accident_id) |>
      as.character()
    
    map_selection_polygon_vec <- 
      do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})) |> 
      as.vector() |>
      as.character()
    
    session$sendCustomMessage("map_polygon", map_selection_polygon_vec)
    session$sendCustomMessage("map_selection", map_selection_vec)
    
    leafletProxy("mpacc") |>
      clearGroup("draw_selection") |> 
      clearGroup("draw") |>
      addPolygons(
        data = map_selection_polygon,
        color = "#ff0066",
        group = "draw_selection"
      )
    
    updateActionButton(session, "removefilter", label = "Smazat filtr")
  })
  
  observeEvent(input$removefilter,{
    leafletProxy("mpacc") %>% clearGroup("draw") %>% clearGroup("draw_selection") 
    session$sendCustomMessage("map_selection", "none")
    updateActionButton(session, "removefilter", label = "Filtr není aktivní")
  })
  
  #### Reports ####
  
  output$report_overview <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_overview.Rmd")
      file.copy("report_overview.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        district = names(MENU$district)[MENU$district == input$menu_district],
        data_accidents_p1 = get_accidents_p1(),
        data_accidents_p2 = get_accidents_p2(),
        period = input$menu_period,
        accfilter = input$menu_filteraccidents
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)
