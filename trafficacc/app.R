# Load libraries, functions, and pre-defied variables
source("functions.R")
source("dictionary.R")

OPTIONS <- list_options()

# MAXIMUM_DATE <- 
#   OPTIONS$period_end %>% 
#   lubridate::as_date() %>% 
#   max() %>% 
#   first()

# Just for the public app
MAXIMUM_DATE <- lubridate::as_date("2022-12-31")

PERIOD_preselected <- OPTIONS |> 
  dplyr::slice_max(period_end, n = 1L) |>
  dplyr::slice(1L) |>
  dplyr::pull(period_menu)

map_districts <- 
  readr::read_rds(
    stringr::str_c(APPDATA_REPOSITORY,"districts.rds")
  ) %>% 
  sf::st_transform(4326)

# map_orp <- 
#   readr::read_rds(
#     stringr::str_c(APPDATA_REPOSITORY,"orps.rds")
#   ) %>% 
#   sf::st_transform(4326)

# Define UI for application that draws a histogram
#### UI ####
ui <- dashboardPage(
  dashboardHeader(
    title = TITLE$apptitle,
    titleWidth = '100%',
    disable = TRUE
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(TITLE$tab1, tabName = "overview", icon = icon("satellite")),
      menuItem(TITLE$tab2, tabName = "hotspots", icon = icon("map-location-dot")),
      menuItem(TITLE$tab3, tabName = "accidents", icon = icon("car-burst"))
    ),
    sidebarMenu(
      selectInput(
        "menu_district", TITLE$district,
        MENU$district[MENU$district %in% OPTIONS$district],
        selected = "CZ0642",
        multiple = FALSE,
        width = '100%'
      ),
      # switchInput(
      #   "lang_switch",
      #   label = "<i class=\"fa-solid fa-language\"></i>",
      #   offLabel = "CZE",
      #   onLabel = "ENG",
      #   width = '100%',
      #   value = FALSE
      # ),
      img(src='muni-lg-white.png', align = "center", width = "100%"),br(),
      img(src='muni-lg-cze-black.png', align = "center", width = "100%"),br(),
      img(src='cuni.png', align = "center", width = "100%"),br(),
      img(src='Doprava.png', align = "center", width = "100%"),br()
    ),
    width = 200,
    collapsed = FALSE
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
    ##### UI: Overview #####
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                column(5,
                       box(
                         h1(textOutput("header_title")),
                         h4(textOutput("header_period")),
                         h4(textOutput("header_period_p2")),
                         h4(textOutput("header_filteraccidents")),
                         width = 12
                       )
                ),
                column(5,
                       box(
                         dateRangeInput(
                           "menu_period",
                           TITLE$base_period,
                           start = str_c(year(MAXIMUM_DATE),"-01-01"),
                           end = MAXIMUM_DATE,#str_c(year(MAXIMUM_DATE),"-12-31"),
                           min = MINIMUM_DATE,
                           max = MAXIMUM_DATE,
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = TITLE$period_separator,
                           width = '100%'
                         ),
                         materialSwitch(
                           "menu_period2_user",
                           TITLE$comp_period,
                           inline = TRUE,
                           value = FALSE,
                           width = '100%'
                         ),
                         dateRangeInput(
                           "menu_period2",
                           label = NULL,
                           start = str_c(year(MAXIMUM_DATE)-1,"-01-01"),
                           end = str_c(year(MAXIMUM_DATE)-1,"-12-31"),
                           min = MINIMUM_DATE,
                           max = MAXIMUM_DATE,
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = TITLE$period_separator,
                           width = '100%'
                         ),
                         actionLink("help_periodovr",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         selectInput(
                           "menu_filteraccidents",
                           TITLE$acc_selection,
                           MENU$filteraccidents,
                           selected = "all",
                           width = '100%'
                         ),
                         status = "warning",
                         width = 12
                       )
                )#,
                # column(2,
                #        box(
                #          p(TITLE$report),
                #          downloadButton("report_overview", "Report"),
                #          uiOutput("reportset_button"),br(),
                #          #downloadButton("reportset", "Pravidelný reporting"),br(),
                #          actionLink("help_report",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                #          status = "warning",
                #          width = 12
                #        )
                # )
              ),
              fluidRow(
                column(3,
                       box(
                         valueBoxOutput("box_acc_n", width = 12),
                         valueBoxOutput("box_acc_n_delta", width = 12),
                         title = TITLE$box_accidents,
                         width = 12
                       )
                ),
                column(3,
                       box(
                         valueBoxOutput("box_death_n", width = 12),
                         valueBoxOutput("box_death_n_delta", width = 12),
                         title = TITLE$box_deaths,
                         width = 12
                       )
                ),
                column(3,
                       box(
                         valueBoxOutput("box_swound_n", width = 12),
                         valueBoxOutput("box_swound_n_delta", width = 12),
                         title = TITLE$box_swound,
                         width = 12
                       )
                ),
                column(3,
                       box(
                         valueBoxOutput("box_lwound_n", width = 12),
                         valueBoxOutput("box_lwound_n_delta", width = 12),
                         title = TITLE$box_lwound,
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_casualties"),
                         title = TITLE$fig_casualties,
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_faults"),
                         title = TITLE$fig_faults,
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_causes"),
                         title = TITLE$fig_causes,
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_mosttragic"),
                         title = TITLE$fig_mosttragic,
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_crashtype"),
                         title = TITLE$fig_crashtype,
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_obstacle"),
                         title = TITLE$fig_obstacle,
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(6,
                       box(
                         plotOutput("fig_age"),
                         title = TITLE$fig_age,
                         width = 12
                       )
                ),
                column(6,
                       box(
                         plotOutput("fig_agedead"),
                         title = TITLE$fig_agedead,
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         plotOutput("fig_timedist"),
                         title = TITLE$fig_timedist,
                         width = 12
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         AUTHORS$line1,
                         em(AUTHORS$line2),br(),
                         AUTHORS$line3,
                         width = 12
                       )
                       )
              )
      ),
      ##### UI: Hotspots #####
      tabItem(tabName = "hotspots",
              fluidRow(
                column(6,
                       box(
                         selectInput(
                           "menu_period_hotspots",
                           TITLE$base_periodprofile_fixed,
                           unique(OPTIONS$period_menu),
                           selected = PERIOD_preselected,
                           width = '100%'
                         ),
                         actionLink("help_period_clusters",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                # column(2,
                #        box(
                #          selectInput(
                #            "menu_profile",
                #            TITLE$menu_profile,
                #            MENU$profile[MENU$profile %in% OPTIONS$profile],
                #            selected = "cost",
                #            width = '100%'
                #          ),
                #          actionLink("help_profile",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                #          status = "warning",
                #          width = 12
                #        )
                # ),
                column(3,
                       box(
                         sliderInput(
                           "menu_quantile",
                           TITLE$menu_quantile,
                           min = 1,
                           max = 25,
                           step = 2,
                           value = 5,
                           ticks = TRUE,
                           width = '100%'
                         ),
                         actionLink("help_severity",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         sliderInput(
                           "menu_spill",
                           TITLE$menu_spill,
                           min = 1,
                           max = 10,
                           value = 2,
                           step = 1,
                           ticks = TRUE,
                           width = '100%'
                         ),
                         actionLink("help_spill",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                )#,
                # column(3,
                #        box(
                #          p(TITLE$note_report),
                #          downloadButton("report_cluster", "Report"),
                #          downloadButton("downloadPOLY", "GeoJSON"),
                #          status = "warning",
                #          width = 12
                #        )
                #        )
              ),
      fluidRow(
        column(6,
               box(
               leafletOutput("mphot", height = 900),
               title = TITLE$mphot,
               width = 12
               ),
               box(
                 tableOutput("profile_desc"),
                 title = TITLE$profiledesc,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 width = 12
               )
               ),
        column(6,
               box(
                 selectInput(
                   "menu_sorting",
                   TITLE$menu_sorting,
                   choices = MENU$sorting,
                   selected = "costs",
                   width = '100%'
                 ),
                 plotOutput(
                   "fig_clusters",
                   height = "250px"
                   ),
                 uiOutput("controlSorting"),
                 actionLink("help_sorting",TITLE$help,icon = icon("circle-info", lib="font-awesome")),
                 status = "warning",
                 width = 12
               ),
               valueBoxOutput("box_acc_n_hot", width = 3),
               valueBoxOutput("box_death_n_hot", width = 3),
               valueBoxOutput("box_swound_n_hot", width = 3),
               valueBoxOutput("box_lwound_n_hot", width = 3),
               box(
                 tableOutput("tab_casualties_hot"), 
                 title = TITLE$fig_casualties,
                 width = 12
               ),
               box(
                 tableOutput("tab_fault_hot"), #tab_causes_accidents
                 title = TITLE$fig_faults,
                 width = 12
               ),
               box(
                 tableOutput("tab_causes_hot"), #tab_type_accidents
                 title = TITLE$fig_causes,
                 width = 12
               ),
               box(
                 tableOutput("tab_type_hot"), #tab_type_accidents
                 title = TITLE$fig_crashtype,
                 width = 12
               ),
               box(
                 plotOutput("fig_age_hot"),
                 title = TITLE$fig_age,
                 width = 12
               )
        )
        ),
      fluidRow(
        column(12,
               box(
                 AUTHORS$line1,
                 em(AUTHORS$line2),br(),
                 AUTHORS$line3,
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
                column(3,
                       box(
                         dateRangeInput(
                           "menu_period_accidents",
                           TITLE$base_period_fixed,
                           start = "2021-01-01",#str_c(year(today())-1,"-01-01"),
                           end = MAXIMUM_DATE,#str_c(year(today())-1,"-12-31"),
                           min = MINIMUM_DATE,
                           max = MAXIMUM_DATE,#str_c(year(today()),"-12-31"),
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = " do ",
                           width = '100%'
                         ),
                         status = "warning",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         selectInput(
                           "menu_filteraccidents_accidents",
                           TITLE$acc_selection,
                           MENU$filteraccidents,
                           selected = "all",
                           width = '100%'
                         ),
                         #actionLink("help_filteraccidents","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         TITLE$filter_cluster,
                         actionLink("cluster_zoom","Zoom",icon = icon("magnifying-glass-location", lib="font-awesome")),
                         materialSwitch(
                           "menu_filteraccidents_cluster",
                           "",
                           value = FALSE,
                           width = '100%'
                         ),
                         actionLink("help_filtercluster","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         TITLE$filter_polygon,
                         actionLink("polygon_zoom","Zoom",icon = icon("magnifying-glass-location", lib="font-awesome")),
                         materialSwitch(
                           "menu_filteraccidents_polygon",
                           "",
                           value = FALSE,
                           width = '100%'
                         ),
                         actionLink("help_filterpolygon","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                )#,
                # column(3,
                #        box(
                #          p(TITLE$note_report),
                #          downloadButton("report_accidents", "Report"),
                #          downloadButton("downloadID", "ID nehod"),
                #          status = "warning",
                #          width = 12
                #        )
                #        )
              ),
              fluidRow(
                column(7,
                       box(
                            leafletOutput("mpacc", height = 900),
                            title = TITLE$mpacc,
                            width = 12
                          ),
                      box(
                            radioButtons("mpacc_legend", 
                                         label = TITLE$mpacc_label,
                            choices = mpacc_choices,
                            selected = "nasledky"),
                            status = "warning",
                            width = 6
                            ),
                      box(
                        mpacc_box
                      )
                        ),
              column(5,
                        valueBoxOutput("box_acc_n_box", width = 3),
                        valueBoxOutput("box_death_n_box", width = 3),
                        valueBoxOutput("box_swound_n_box", width = 3),
                        valueBoxOutput("box_lwound_n_box", width = 3),
                     box(
                       tableOutput("tab_casualties_accidents"), 
                       title = TITLE$fig_casualties,
                       width = 12
                     ),
                     box(
                       tableOutput("tab_fault_accidents"), #tab_causes_accidents
                       title = TITLE$fig_faults,
                       width = 12
                     ),
                     box(
                       tableOutput("tab_causes_accidents"), #tab_type_accidents
                       title = TITLE$fig_causes,
                       width = 12
                     ),
                     box(
                       tableOutput("tab_type_accidents"), #tab_type_accidents
                       title = TITLE$fig_crashtype,
                       width = 12
                     ),
                     box(
                       plotOutput("fig_age_box"),
                       title = TITLE$fig_age,
                       width = 12
                     )
              )
          ),
          fluidRow(
            column(12,
                   box(
                     AUTHORS$line1,
                     em(AUTHORS$line2),br(),
                     AUTHORS$line3,
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
  
  #### Landing screen
  # observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = input$click, { 
  #   showModal(modalDialog(
  #     title = "Aplikace pro interaktivní analýzu dopravních nehod", 
  #     p("Toto je ukázková verze aplikace, která byla vyvinuta pro potřeby Policie ČR v rámci projektu 'TA ČR (CK01000049): Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR'."),
  #     p("Aplikace je primárně určena pro dopravní experty, kteří dokážou údaje správně interpretovat. Ukázková verze aplikace obsahuje data pouze pro vybrané okresy ČR a využívá pouze volně dostupná data."),
  #     h4("Co aplikace umí?"),
  #     p("Aplikace nabízí tři pohledy na data. Na tabu 'Přehled' vidí uživatel souhrnné statistiky pro vybraný okres a období."),
  #     p("Tab 'Nehodové lokality' ukazuje identifikované nehodové lokality a statistiky pro uživatelem vybranou nehodovou lokalitu (klastr). Nehodové lokality jsou identifikovány pomocí inovativní technologie, která umožňuje analýzu nehod na celé silniční síti."),
  #     p("Poslední tab 'Dopravní nehody' dává uživateli největší volnost. Může pracovat se statistikami nehod z libovolného úseku dopravní sítě a libovolného období. But with a great power comes great responsibility."),
  #     h4("Poznámka"),
  #     p("V základním nastavení aplikace zobrazuje okres Brno-město. It's not a bug. It's a feature."),
  #     footer = modalButton("Rozumím. Přejít na aplikaci.")
  #   ))
  # })
  
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = input$click, {
    showModal(modalDialog(
      title = "Aplikace pro interaktivní analýzu dopravních nehod a nehodových míst",
      p("Toto je ukázková verze aplikace, která byla vyvinuta pro potřeby Policie ČR v rámci projektu 'TA ČR (CK01000049): Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR'."),
      p("Aplikace je primárně určena pro dopravní experty, kteří dokážou údaje správně interpretovat. Ukázková verze aplikace obsahuje pouze vybrané údaje, které striktně vycházejí z veřejně dostupných dat."),
      h4("Co aplikace umí?"),
      p("1. Ukazuje statistiky dopravních nehod pro zvolené okresy."),
      p("2. Vytipovává a ukazuje potenciálně problematická místa na silniční síti v ČR."),
      p("3. Umožňuje základní interaktivní analýzu dopravních nehod."),
      h4("Kontakty (MUNI)"),
      p("Michal Kvasnička (michal.kvasnicka@econ.muni.cz, backend a indentifikace shluků nehod) a Štěpán Mikula (stepan.mikula@econ.muni.cz, aplikace)."),
      h4("Upozornění"),
      p("Aplikace má primárně sloužit jako podpůrný nástroj pro expertní rozhodování. Prezentovaná data vyžadují odbornou interpretaci. Autoři aplikace nenesou odpovědnost za použití aplikace a prezentovaných výstupů a dat."),
      footer = modalButton("Rozumím. Přejít na aplikaci.")
    ))
  })
  
  ##### Help #####
  
  # help_spill
  # help_period_clusters
  # help_profile
  # help_severity
  # help_sorting
  # help_filteraccidents
  # help_filtercluster
  # help_filterpolygon
  # help_periodovr
  
  observeEvent(input$help_spill, {
    shinyalert(TITLE$help, HELP$spill_text, type = "info")
  })
  
  observeEvent(input$help_period_clusters, {
    shinyalert(TITLE$help, HELP$period_clusters_text, type = "info")
  })
  
  observeEvent(input$help_profile, {
    shinyalert(TITLE$help, HELP$profile_text, type = "info")
  })
  
  observeEvent(input$help_severity, {
    shinyalert(TITLE$help, HELP$severity_text, type = "info")
  })
  
  observeEvent(input$help_sorting, {
    shinyalert(TITLE$help, HELP$sorting_text, type = "info")
  })
  
  observeEvent(input$help_filtercluster, {
    shinyalert(TITLE$help, HELP$filtercluster_text, type = "info")
  })
  
  observeEvent(input$help_filterpolygon, {
    shinyalert(TITLE$help, HELP$filterpolygon_text, type = "info")
  })
  
  observeEvent(input$help_periodovr, {
    shinyalert(TITLE$help, HELP$periodovr_text, type = "info")
  })
  
  observeEvent(input$help_report, {
    shinyalert(TITLE$help, HELP$report_text, type = "info")
  })
  
  
  
  ##### Header #####
  
  output$header_title <- renderText({
    stringr::str_c(TITLE$header_title,
                   names(MENU$district)[MENU$district == input$menu_district])
  })
  
  output$header_period <- renderText({
    # # Duration of p1 in days
    # p2_length <- input$menu_period[2] - input$menu_period[1]
    # p2_length <- as.double(p2_length)
    # 
    # # End date of comparison period (p2)
    # p2_end <- input$menu_period[1]
    # p2_end <- p2_end - 1
    # 
    # # Start date of comparison period (p2)
    # p2_start <- p2_end - p2_length
    # 
    # p2_string <- stringr::str_c(
    #   strftime(p2_start, format = "%d.%m.%Y"),
    #   " - ",
    #   strftime(p2_end, format = "%d.%m.%Y")
    # )
    p1_start <- input$menu_period[1]
    p1_end   <- input$menu_period[2]
    
    if(p1_end < p1_start){
      p1_start <- input$menu_period[2]
      p1_end <- input$menu_period[1]
      
      shinyalert::shinyalert(
        title = ALERT$title1,
        type = "warning",
        text = ALERT$text1
      )
    }
    
    
    stringr::str_c("Základní období: ",
                   strftime(p1_start, format = "%d.%m.%Y"),
                   " - ",
                   strftime(p1_end, format = "%d.%m.%Y")
    )
  })
  
  output$header_period_p2 <- renderText({
    # Duration of p1 in days
    p1_start <- input$menu_period[1]
    p1_end <- input$menu_period[2]
    
    if(p1_end < p1_start){
      p1_start <- input$menu_period[2]
      p1_end <- input$menu_period[1]
    }
    
    p1_length <- p1_end - p1_start
    p1_length <- as.double(p1_length) |> as.integer()
    
    
    if(input$menu_period2_user){
    
      p2_start <- input$menu_period2[1]
      p2_end <- input$menu_period2[2]
    
      if(p2_end < p2_start){
        p2_start <- input$menu_period2[2]
        p2_end <- input$menu_period2[1]
        
        shinyalert::shinyalert(
          title = ALERT$title1,
          type = "warning",
          text = ALERT$text1
        )
      }
      
    }else{
      
      # End date of comparison period (p2)
      p2_end <- p1_start
      p2_end <- p2_end - 1
      
      # Start date of comparison period (p2)
      p2_start <- p2_end - p1_length
      
    }
    
    p2_string <- stringr::str_c(
      strftime(p2_start, format = "%d.%m.%Y"),
      " - ",
      strftime(p2_end, format = "%d.%m.%Y")
    )
    
    
    p2_length <- p2_end - p2_start
    p2_length <- as.double(p1_length) |> as.integer()
    
    if(p2_length != p1_length & input$menu_period2_user){
      shinyalert::shinyalert(
        title = ALERT$title2,
        type = "warning",
        text = ALERT$title2_1
        )
    }
    
    if(p2_end >= p1_start & p2_end <= p1_end){
      shinyalert::shinyalert(
        title = ALERT$title3,
        type = "warning",
        text = ALERT$text3
      )
    }
    
    if(p2_start >= p1_end){
      shinyalert::shinyalert(
        title = ALERT$title4,
        type = "warning",
        text = ALERT$text4
      )
    }
    
    
    if(p2_start < MINIMUM_DATE){
      shinyalert::shinyalert(
        title = ALERT$title2,
        type = "warning",
        text = stringr::str_c(ALERT$text2_2a,p2_string,ALERT$text2_2b)
      )
      
      p2_start <- MINIMUM_DATE
      
      p2_string <- stringr::str_c(
        strftime(p2_start, format = "%d.%m.%Y"),
        " - ",
        strftime(p2_end, format = "%d.%m.%Y")
      )
    }
    
    
    stringr::str_c(TITLE$comp_period_set,
                   p2_string
    )
  })
  
  output$header_filteraccidents <- renderText({
    stringr::str_c(TITLE$filteracc,
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
      ) |>
      dplyr::filter(accident_inside_the_district)
    
    out <- out |>
      dplyr::mutate(
        label = str_c(
          "<b>",ACCCHAR$id,"</b>", accident_id,"<br>",
          "<b>",ACCCHAR$date,"</b>", strftime(accident_date, format = "%d.%m.%Y"),"<br>",
          "<b>",ACCCHAR$type,"</b>", crashtype_nolinebreaks[as.character(accident_type)],"<br>",
          "<b>",ACCCHAR$dead,"</b>", accident_dead,"<br>",
          "<b>",ACCCHAR$swound,"</b>", accident_serious_injury,"<br>",
          "<b>",ACCCHAR$lwound,"</b>", accident_light_injury,"<br>",
          "<b>",ACCCHAR$damage,"</b>", format(1e6*accident_material_cost, nsmall=0, trim=TRUE, big.mark=" ")
        )
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
  get_accidents_district <- reactive({
    
    out <- read_accidents(
      district = input$menu_district
    )
    
    out <- out |>
      dplyr::mutate(
        label = str_c(
          "<b>",ACCCHAR$id,"</b>", accident_id,"<br>",
          "<b>",ACCCHAR$date,"</b>", strftime(accident_date, format = "%d.%m.%Y"),"<br>",
          "<b>",ACCCHAR$type,"</b>", crashtype_nolinebreaks[as.character(accident_type)],"<br>",
          "<b>",ACCCHAR$dead,"</b>", accident_dead,"<br>",
          "<b>",ACCCHAR$swound,"</b>", accident_serious_injury,"<br>",
          "<b>",ACCCHAR$lwound,"</b>", accident_light_injury,"<br>",
          "<b>",ACCCHAR$damage,"</b>", format(1e6*accident_material_cost, nsmall=0, trim=TRUE, big.mark=" ")
        )
      )
    
    return(out)
    
  })
  
  ###### Get data: Box ######
  get_accidents_box <- reactive({
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(
        accident_date >= input$menu_period_accidents[1]
      ) |>
      dplyr::filter(
        accident_date <= input$menu_period_accidents[2]
      ) |>
      dplyr::filter(accident_inside_the_district)
    
    # if(length(input$map_selection) != 0){
    #   if(input$map_selection[1] != "none"){
    #   out <-
    #     out |>
    #       dplyr::filter(
    #         accident_id %in% input$map_selection
    #       )
    #   }
    # }
    
    if(input$menu_filteraccidents_polygon){
    if(length(input$mpacc_draw_new_feature) != 0){
      
          map_selection_polygon <- 
            get_selectedPOLY() |>
            sf::st_transform(
              sf::st_crs(out)
            )
          
          out <- 
            st_filter(
              out,map_selection_polygon
            )
      #}
      #}
    }
    }
    
    
    
    if(input$menu_filteraccidents_cluster){
      out <-
        sf::st_filter(
          x = out,
          y = get_clusterPOLY()
          )
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
    
    p1_start <- input$menu_period[1]
    p1_end <- input$menu_period[2]
    
    if(p1_end < p1_start){
      p1_start <- input$menu_period[2]
      p1_end <- input$menu_period[1]
    }
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(accident_inside_the_district) |>
      dplyr::filter(
        accident_date >= p1_start
      ) |>
      dplyr::filter(
        accident_date <= p1_end
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
    
    
    if(input$menu_period2_user){
      p2_start <- input$menu_period2[1]
      p2_end <- input$menu_period2[2]
      
      if(p2_end < p2_start){
        p2_start <- input$menu_period2[2]
        p2_end <- input$menu_period2[1]
      }
      
    }else{
    
      p1_start <- input$menu_period[1]
      p1_end <- input$menu_period[2]
      
      if(p1_end < p1_start){
        p1_start <- input$menu_period[2]
        p1_end <- input$menu_period[1]
      }
      
    # Duration of p1 in days
    p2_length <- p1_end - p1_start
    p2_length <- as.double(p2_length)
    
    # End date of comparison period (p2)
    p2_end <- p1_start
    p2_end <- p2_end - 1
    
    # Start date of comparison period (p2)
    p2_start <- p2_end - p2_length
    }
    
    
    out <- read_accidents(
      district = input$menu_district
    ) |>
      dplyr::filter(accident_inside_the_district) |>
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
      pr, BOXTITLE$period_baseline, 
      color = "light-blue",
      icon = icon("car-burst")
    )
  })
  
  output$box_acc_n_delta <- renderValueBox({
    n1 <- nrow(get_accidents_p1())
    n2 <- nrow(get_accidents_p2())
    
    pr <- get_delta(n1,n2)
    
    valueBox(
      pr, BOXTITLE$period_change, 
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
      pr, BOXTITLE$period_baseline, 
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
      pr, BOXTITLE$period_change, 
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
      pr, BOXTITLE$period_baseline, 
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
      pr, BOXTITLE$period_change, 
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
      pr, BOXTITLE$period_baseline, 
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
      pr, BOXTITLE$period_change, 
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
        FIGS$casualties
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = FIGS$period_baseline,
          "p2" = FIGS$period_comparison
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
        FIGS$accidents
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = FIGS$period_baseline,
          "p2" = FIGS$period_comparison
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
        FIGS$accidents
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
        FIGS$dead
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
          "p1" = FIGS$period_baseline,
          "p2" = FIGS$period_comparison
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
        FIGS$dead
      ) +
      scale_fill_brewer(
        palette = "Set1",
        breaks = c("p1","p2"),
        labels = c(
          "p1" = FIGS$period_baseline,
          "p2" = FIGS$period_comparison
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
        FIGS$age
      ) +
      scale_y_continuous(
        FIGS$accidents
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
        FIGS$age
      ) +
      scale_y_continuous(
        FIGS$dead
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
  
  output$fig_timedist <- renderPlot({
    
    aux <- get_accidents_p1() %>% 
      st_drop_geometry() %>% 
      group_by(accident_date) %>% 
      summarise(
        obs = n(),
        .groups = "drop"
      ) %>% 
      mutate(
        wday = wday(accident_date, week_start = 1) %>% as.integer(),
        month = month(accident_date) %>% as.integer(),
        year = year(accident_date) %>% as.integer()
      ) %>% 
      group_by(year,month) %>% 
      arrange(accident_date) %>% 
      mutate(
        lday = first(wday):(n()+first(wday)-1)
      ) %>% 
      group_by(wday,month,lday) %>% 
      summarise(
        obs = sum(obs)/n(),
        .groups = "drop"
      ) %>% 
      mutate(
        obs = 100*obs/sum(obs),
        month = factor(month, levels = 1:12, labels = months_cz)
      ) 
    
    aux %>%
      ggplot(
        aes(x = lday, y = month, fill = obs)
      ) +
      geom_tile() +
      geom_vline(
        xintercept = c(7.5,14.5,21.5,28.5,35.5),
        linetype = 2
      ) +
      scale_x_continuous(
        breaks = 1:37,
        labels = rep(
          days_cz,6
        )[1:37]
      ) +
      scale_y_discrete(
        limits = months_cz
      ) +
      scale_fill_distiller(
        "Podíl\nnehod (%)",
        palette = "RdYlGn"
      ) +
      # scale_fill_gradient2(
      #   "Podíl nehod (%)",
      #   low = "#313695",
      #   mid = "#ffffbf",
      #   high = "#a50026",
      #   midpoint = median(aux$obs, na.rm = TRUE)
      # ) +
      theme_classic(base_size = 16) +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, size = 12),
        axis.title = element_blank()
        #legend.position = "bottom"
      )
    
    # acc_plot <- 
    #   get_accidents_p1() |> 
    #   dplyr::select(accident_date) |> 
    #   sf::st_drop_geometry() |>
    #   dplyr::group_by(accident_date) |> 
    #   dplyr::summarise(obs = n()) %>%  
    #   tidyr::complete(
    #     accident_date = seq(from = min(.$accident_date), to = max(.$accident_date), by = 1),
    #     fill = list(obs = 0)
    #   ) |> 
    #   dplyr::mutate(
    #     day  = wday(accident_date, week_start = 1) %>% as.character(),
    #     week = isoweek(accident_date) %>% as.character(),
    #   ) |> 
    #   dplyr::select(-accident_date) |> 
    #   dplyr::group_by(week,day) |> 
    #   dplyr::summarise(
    #     obs = sum(obs)/n(),
    #     .groups = "drop"
    #   ) |> 
    #   dplyr::mutate(
    #     obs = 100*obs/sum(obs)
    #   ) |> 
    #   dplyr::mutate(
    #     week = factor(week, levels = 1:54),
    #     day = factor(day, levels = 1:7, 
    #                  labels = c("pondělí","úterý","středa","čtvrtek","pátek","sobota","neděle"))
    #   )
    # 
    # xdata <- acc_plot |> 
    #   group_by(week) |> 
    #   summarise(
    #     obs = sum(obs),
    #     .groups = "drop"
    #   ) |> 
    #   mutate(
    #     xobs = 100*obs/sum(obs)
    #   ) |> 
    #   select(-obs)
    # 
    # ydata <- acc_plot |> 
    #   group_by(day) |>
    #   summarise(
    #     obs = sum(obs),
    #     .groups = "drop"
    #   ) |> 
    #   mutate(
    #     yobs = 100*obs/sum(obs)
    #   ) |>
    #   select(-obs)
    # 
    # acc_plot |>
    #   ggplot(
    #     aes(x = week)
    #   ) +
    #   geom_tile(
    #     aes(y=day, fill=obs)
    #   ) +
    #   geom_xsidetile(
    #     data = xdata,
    #     aes(xfill = xobs, y = "celkem")
    #   ) +
    #   geom_ysidetile(
    #     data = ydata,
    #     aes(yfill = yobs, x = "celkem", y = day)
    #   ) +
    #   scale_fill_distiller(
    #     "Za den v roce (%)",
    #     palette = "OrRd",
    #     direction = 1,
    #     guide = guide_colorbar(order = 1)
    #   ) +
    #   scale_xfill_gradient(
    #     "Za týden v roce (%)",
    #     low = "#deebf7",
    #     high = "#08306b"
    #   ) +
    #   scale_yfill_gradient(
    #     "Za den v týdnu (%)",
    #     low = "#deebf7",
    #     high = "#08306b"
    #   ) +
    #   scale_y_discrete(
    #     limits = c("pondělí","úterý","středa","čtvrtek","pátek","sobota","neděle")
    #   ) + 
    #   scale_xsidey_discrete() +
    #   scale_ysidex_discrete() +
    #   scale_x_discrete(
    #     "Týden v roce",
    #     limits = as.character(1:53)
    #   ) + 
    #   theme_classic(
    #     base_size = 16
    #   ) +
    #   theme(
    #     axis.title.y = element_blank(),
    #     legend.position = "bottom"
    #   )
  })
  
  ##### Hotspots #####
  
  ###### Read clusters #####
  get_clusters <- reactive({
    
    out <- read_clusters(
      district = input$menu_district,
      #profile = input$menu_profile,
      profile = first(OPTIONS$profile[OPTIONS$period_menu == input$menu_period_hotspots]),
      period_start = first(OPTIONS$period_start[OPTIONS$period_menu == input$menu_period_hotspots]),
      period_end = first(OPTIONS$period_end[OPTIONS$period_menu == input$menu_period_hotspots])
    )
    
    out <- out |>
      #dplyr::filter(severity == 1-(input$menu_quantile/1000)) |>
      dplyr::filter(severity == input$menu_quantile) |>
      dplyr::filter(additional_lixels == input$menu_spill)
    
    clusters <- list()
    clusters$accidents <- out$accidents[[1]]
    clusters$clusters <- out$clusters[[1]]
    
    if(input$menu_sorting == "cost"){
      sorting_rule <- 
        clusters$clusters |>
        sf::st_drop_geometry() |>
        dplyr::arrange(desc(cost)) |>
        dplyr::mutate(
          cluster_sorted = row_number()
        ) |>
        dplyr::select(
          cluster, cluster_sorted, ordervalue = cost
        )
    }else{
      sorting_rule <- 
        clusters$clusters |>
        sf::st_drop_geometry() |>
        dplyr::arrange(desc(cost_per_meter)) |>
        dplyr::mutate(
          cluster_sorted = row_number()
        ) |>
        dplyr::select(
          cluster, cluster_sorted, ordervalue = cost_per_meter
        )
    }
    
    clusters$clusters <- 
      dplyr::left_join(clusters$clusters,sorting_rule, by = "cluster") |>
      dplyr::select(-cluster) |>
      dplyr::rename(cluster = cluster_sorted)
    
    clusters$accidents <- 
      dplyr::left_join(clusters$accidents,sorting_rule, by = "cluster") |>
      dplyr::select(-cluster) |>
      dplyr::rename(cluster = cluster_sorted)
    
    # Total number of clusters
    clusters$N <- nrow(clusters$clusters)
    
    return(clusters)
  })
  
  ###### UI ######
  
  # output$controlPeriod <- renderUI({
  #   
  #   items <- OPTIONS |>
  #     dplyr::filter(
  #       district == input$menu_district
  #     )
  #   
  #   tagList(
  #     selectInput(
  #       "menu_period_hotspots",
  #       TITLE$base_periodprofile_fixed,
  #       unique(items$period_menu),
  #       selected = PERIOD_preselected,
  #       width = '100%'
  #     )
  #   )
  #   
  # })
  
  output$controlSorting <- renderUI({
    
    maxN <- get_clusters()
    
    tagList(
      sliderInput("menu_cluster", 
                  TITLE$menu_cluster, 
                  min = 1,
                  max = maxN$N,
                  step = 1,
                  value = 1,
                  width = '100%'
      )
    )
  })
  
  ###### Boxes #####
  
  output$profile_desc <- renderTable(
    width = "100%",
    striped = FALSE,
    colnames = FALSE,
    align = "lr",
    {
    
    sidecar <- read_sidecar(
      district = input$menu_district,
      #profile = input$menu_profile,
      profile = first(OPTIONS$profile[OPTIONS$period_menu == input$menu_period_hotspots]),
      period_start = first(OPTIONS$period_start[OPTIONS$period_menu == input$menu_period_hotspots]),
      period_end = first(OPTIONS$period_end[OPTIONS$period_menu == input$menu_period_hotspots])
    ) |>
      dplyr::mutate(
        across(
          where(
            is.double
          ),
          format,
          nsmall = 1,
          big.mark = " ",
          digits = 1,
          trim = TRUE,
          scientific = FALSE
        )
      ) |>
      dplyr::mutate(
        across(
          everything(),
          as.character
        )
      )
      
    sidecar <- tibble(
      name = names(PROFILEDESC)[!names(PROFILEDESC) %in% names(sidecar)],
      value = ""
    ) %>% 
      pivot_wider() %>% 
      bind_cols(sidecar,.)
    
    tibble::tribble(
      ~label, ~value,
      PROFILEDESC$PROFILE_COMMENT, sidecar$PROFILE_COMMENT,
      PROFILEDESC$NKDE_METHOD, sidecar$NKDE_METHOD,
      PROFILEDESC$UNIT_COST_CONST, sidecar$UNIT_COST_CONST, 
      PROFILEDESC$UNIT_COST_DEAD, sidecar$UNIT_COST_DEAD, 
      PROFILEDESC$UNIT_COST_SERIOUS_INJURY, sidecar$UNIT_COST_SERIOUS_INJURY, 
      PROFILEDESC$UNIT_COST_LIGHT_INJURY, sidecar$UNIT_COST_LIGHT_INJURY, 
      PROFILEDESC$UNIT_COST_MATERIAL, sidecar$UNIT_COST_MATERIAL, 
      PROFILEDESC$CONST_COST_DEAD, sidecar$CONST_COST_DEAD,
      PROFILEDESC$CONST_COST_SERIOUS_INJURY, sidecar$CONST_COST_SERIOUS_INJURY,
      PROFILEDESC$CONST_COST_LIGHT_INJURY, sidecar$CONST_COST_LIGHT_INJURY,
      PROFILEDESC$CONST_COST_MATERIAL, sidecar$CONST_COST_MATERIAL,
      #PROFILEDESC$UNIT_COST_SERIOUS_INJURY, sidecar$UNIT_COST_SERIOUS_INJURY,
      #PROFILEDESC$ACCIDENT_TO_ROAD_MAX_DISTANCE, sidecar$ACCIDENT_TO_ROAD_MAX_DISTANCE, 
      #PROFILEDESC$DISTRICT_BUFFER_SIZE, sidecar$DISTRICT_BUFFER_SIZE, 
      PROFILEDESC$LIXEL_SIZE, sidecar$LIXEL_SIZE, 
      #PROFILEDESC$LIXEL_MIN_DIST, sidecar$LIXEL_MIN_DIST,
      PROFILEDESC$NKDE_BW, sidecar$NKDE_BW, 
      PROFILEDESC$NKDE_WEIGHTS, sidecar$NKDE_WEIGHTS,
      #PROFILEDESC$NKDE_AGG, sidecar$NKDE_AGG, 
      PROFILEDESC$NKDE_ADAPTIVE, sidecar$NKDE_ADAPTIVE,
      #PROFILEDESC$SUPPORTED_ROAD_CLASSES, sidecar$SUPPORTED_ROAD_CLASSES,
      PROFILEDESC$NKDE_TRIM_BW, sidecar$NKDE_TRIM_BW
    )
    })
  
  output$box_acc_n_hot <- renderValueBox({
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }
    
    cll <- get_clusters()
    cll <- cll$accidents |>
      dplyr::filter(cluster == selected_cluster)
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(accident_id %in% cll$accident_id)
    
    n1 <- nrow(clusters_accidents)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, BOXTITLE$accidents, 
      color = "light-blue",
      icon = icon("car-burst")
    )
  })
  
  output$box_death_n_hot <- renderValueBox({
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }
    
    cll <- get_clusters()
    cll <- cll$accidents |>
      dplyr::filter(cluster == selected_cluster)
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(accident_id %in% cll$accident_id)
    
    n1 <- clusters_accidents |> 
      dplyr::pull(accident_dead) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, BOXTITLE$dead, 
      color = "light-blue",
      icon = icon("skull-crossbones")
    )
  })
  
  output$box_swound_n_hot <- renderValueBox({
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }
    
    cll <- get_clusters()
    cll <- cll$accidents |>
      dplyr::filter(cluster == selected_cluster)
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(accident_id %in% cll$accident_id)
    
    n1 <- clusters_accidents |> 
      dplyr::pull(accident_serious_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, BOXTITLE$swound, 
      color = "light-blue",
      icon = icon("crutch")
    )
  })
  
  output$box_lwound_n_hot <- renderValueBox({
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }
    
    cll <- get_clusters()
    cll <- cll$accidents |>
      dplyr::filter(cluster == selected_cluster)
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(accident_id %in% cll$accident_id)
    
    n1 <- clusters_accidents |> 
      dplyr::pull(accident_light_injury) |> 
      sum(na.rm = TRUE)
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, BOXTITLE$lwound, 
      color = "light-blue",
      icon = icon("user-injured")
    )
  })
  
  ###### Figures ######
  
  output$fig_clusters <- renderPlot({
    data_clusters <- get_clusters()
    
    data_clusters$clusters |>
      sf::st_drop_geometry() |>
      tidyr::drop_na() |>
      #dplyr::slice_max(ordervalue, n = 30) |>
      ggplot(
        aes(x = cluster, y = ordervalue)
      ) +
      geom_vline(
        xintercept = input$menu_cluster,
        color = "red",
        linetype = 2
      ) +
      geom_line() +
      geom_point() +
      scale_y_continuous(
        FIGS$cluster_weight
      ) +
      scale_x_continuous(
        FIGS$cluster_order
      ) +
      theme_classic(
        base_size = 16
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  output$fig_age_hot <- renderPlot({
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }
    
    cll <- get_clusters()
    cll <- cll$accidents |>
      dplyr::filter(cluster == selected_cluster)
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(accident_id %in% cll$accident_id)
    
      clusters_accidents |>
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
        FIGS$age
      ) +
      scale_y_continuous(
        FIGS$accidents
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
  
  ###### Polygon #####
  
  output$downloadPOLY <- downloadHandler(
    filename = "cluster.GeoJSON",
    content = function(file) {
      data_clusters <- get_clusters()
      
      if(length(input$menu_cluster) == 0){
        selected_cluster <- 1
      }else{
        selected_cluster <- input$menu_cluster
      }
      
      data_clusters$clusters |>
        dplyr::filter(
          cluster == selected_cluster
        ) |>
        sf::st_geometry() |>
        sf::st_union() |>
        sf::st_buffer(10) |>
        #geojsonsf::geojson_sf() |>
        sf::st_write(dsn = file, driver = "GeoJSON")
    }
  )
  
  get_clusterPOLY <- reactive({
    
    data_clusters <- get_clusters()
    
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }
    
    data_clusters$clusters |>
      dplyr::filter(
        cluster == selected_cluster
      ) |>
      sf::st_geometry() |>
      sf::st_union() |>
      sf::st_buffer(10)
    
  })
  
  ###### Maps ######
  
  output$mphot <- renderLeaflet({
    
    data_clusters <- get_clusters()
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(
        accident_id %in% data_clusters$accidents$accident_id
      ) |>
      dplyr::mutate(
        label = str_c(
          "<b>",ACCCHAR$id,"</b>", accident_id,"<br>",
          "<b>",ACCCHAR$date,"</b>", strftime(accident_date, format = "%d.%m.%Y"),"<br>",
          "<b>",ACCCHAR$type,"</b>", crashtype_nolinebreaks[as.character(accident_type)],"<br>",
          "<b>",ACCCHAR$dead,"</b>", accident_dead,"<br>",
          "<b>",ACCCHAR$swound,"</b>", accident_serious_injury,"<br>",
          "<b>",ACCCHAR$lwound,"</b>", accident_light_injury,"<br>",
          "<b>",ACCCHAR$damage,"</b>", format(1e6*accident_material_cost, nsmall=0, trim=TRUE, big.mark=" ")
        )
      )
    
    clusters <- data_clusters$clusters |>
      dplyr::select(
        cluster,ordervalue,X,Y
      )
    
    
    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }

    zoom_clusters <-
      data_clusters$clusters |>
      dplyr::filter(
        cluster == selected_cluster
      ) |>
      dplyr::select(X,Y)
    
    
    leaflet(data = get_map_district()) |>
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |>
      addTiles(group = "OSM (default)") |>
      addProviderTiles('Esri.WorldImagery', group = "Satelite") |>
      addPolygons(
        fill = FALSE
      ) |> 
      # addPolygons(
      #   fill = FALSE,
      #   data = get_map_orp(),
      #   weight = 2
      # ) %>% 
      addPolygons(
        data = get_clusterPOLY(),
        fillColor = "blue",
        color = "#31a354",
        #fillOpacity =,
        smoothFactor = 5
      ) |>
      addPolylines(
        data = sf::st_geometry(clusters),
        group = "Hustoty",
        color = "red"
      ) |>
      addCircleMarkers(
        data = st_jitter(
          clusters_accidents, factor = 0.0002
        ),
        radius = 3,
        color = "black",
        weight = 1,
        fillOpacity = 0.5,
        popup = ~label
      ) |>
      addLabelOnlyMarkers(
        data = clusters,
        lng = ~X,
        lat = ~Y,
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
      ) |>
      setView(
        lng = zoom_clusters$X[1],
        lat = zoom_clusters$Y[1],
        zoom = 16
      )
    
  })
  
  
  ###### Tables ######
  output$tab_casualties_hot <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      
      if(length(input$menu_cluster) == 0){
        selected_cluster <- 1
      }else{
        selected_cluster <- input$menu_cluster
      }
      
      cll <- get_clusters()
      cll <- cll$accidents |>
        dplyr::filter(cluster == selected_cluster)
      
      clusters_accidents <- get_accidents_district() |>
        dplyr::filter(accident_id %in% cll$accident_id)
      
      out <- 
        clusters_accidents %>% 
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
          ALERT$noacc,""
        ))
      }
    })
  
  
  output$tab_fault_hot <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      
      if(length(input$menu_cluster) == 0){
        selected_cluster <- 1
      }else{
        selected_cluster <- input$menu_cluster
      }
      
      cll <- get_clusters()
      cll <- cll$accidents |>
        dplyr::filter(cluster == selected_cluster)
      
      clusters_accidents <- get_accidents_district() |>
        dplyr::filter(accident_id %in% cll$accident_id)
      
      out <- clusters_accidents |>
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
          ALERT$noacc,""
        ))
      }
    })
  
  
  output$tab_causes_hot <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      if(length(input$menu_cluster) == 0){
        selected_cluster <- 1
      }else{
        selected_cluster <- input$menu_cluster
      }
      
      cll <- get_clusters()
      cll <- cll$accidents |>
        dplyr::filter(cluster == selected_cluster)
      
      clusters_accidents <- get_accidents_district() |>
        dplyr::filter(accident_id %in% cll$accident_id)
      
      out <- clusters_accidents |>
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
          ALERT$noacc_cause,""
        ))
      }
    })
  
  output$tab_type_hot <- renderTable(
    width = "100%",
    striped = TRUE,
    colnames = FALSE,
    align = "lr",
    {
      if(length(input$menu_cluster) == 0){
        selected_cluster <- 1
      }else{
        selected_cluster <- input$menu_cluster
      }
      
      cll <- get_clusters()
      cll <- cll$accidents |>
        dplyr::filter(cluster == selected_cluster)
      
      clusters_accidents <- get_accidents_district() |>
        dplyr::filter(accident_id %in% cll$accident_id)
      
      out <- clusters_accidents |>
        sf::st_drop_geometry() |>
        dplyr::select(
          accident_type
        ) |>
        # dplyr::filter(
        #   accident_type != 0
        # ) |>
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
          ALERT$noacc_cause,""
        ))
      }
    })
  
  
  ##### Accidents #####
  
  ###### Boxes #####
  
  output$box_acc_n_box <- renderValueBox({
    n1 <- nrow(get_accidents_box())
    
    pr <- format(n1, trim = FALSE, nsmall = 0, big.mark = " ")
    
    valueBox(
      pr, BOXTITLE$accidents, 
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
      pr, BOXTITLE$dead, 
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
      pr, BOXTITLE$swound, 
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
      pr, BOXTITLE$lwound, 
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
          ALERT$noacc,""
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
          ALERT$noacc_fault,""
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
          ALERT$noacc_cause,""
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
        # dplyr::filter(
        #   accident_type != 0
        # ) |>
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
          ALERT$noacc_cause,""
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
        FIGS$age
      ) +
      scale_y_continuous(
        FIGS$accidents
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
  
  output$downloadID <- downloadHandler(
    filename = "accidentsID.csv",
    content = function(file) {
      
      get_accidents_box() |>
        dplyr::select(p1 = accident_id) |>
        sf::st_drop_geometry() |>
        readr::write_csv(file = file)
      
    }
  )
  
  ###### Maps #####
  get_map_district <- reactive({
    map_districts |>
      filter(district_id == input$menu_district) |>
      st_geometry()
  })
  
  # get_map_orp <- reactive({
  #   map_orp |>
  #     filter(district_id == input$menu_district) |>
  #     st_geometry()
  # })
  
  output$mpacc <- renderLeaflet({
    
    fdata <- get_accidents() |>
      dplyr::mutate(
        nasledky = dplyr::case_when(
          accident_dead > 0 ~ mpacc_levels$dead,
          accident_serious_injury > 0 ~ mpacc_levels$swound,
          accident_light_injury > 0 ~ mpacc_levels$lwound,
          TRUE ~ mpacc_levels$other
        )
      )
    
    #ZOOM <- get_zoom()
    
    #fpal <- colorFactor("Set1", domain = fdata$nasledky, levels = levels(fdata$nasledky))
    fpal <- leaflet::colorFactor("Set1", 
                                 domain = fdata$nasledky,
                                 ordered = TRUE,
                        levels = c(
                          mpacc_levels$dead,
                          mpacc_levels$swound,
                          mpacc_levels$lwound,
                          mpacc_levels$other
                        ))
    
    leaflet(data = get_map_district()) |>
      #leaflet(data = select_()) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |>
      addTiles(group = "OSM (default)") |>
      addProviderTiles('Esri.WorldImagery', group = "Satelite") |>
      addPolygons(
        fill = FALSE
      ) %>% 
      # addPolygons(
      #   fill = FALSE,
      #   data = get_map_orp(),
      #   weight = 2
      # ) %>% 
      addPolygons(
        fill = FALSE,
        layerId = "selectedPOLY",
        data = get_selectedPOLY()
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
      addPolygons(
        data = get_clusterPOLY(),
        fillColor = "blue",
        color = "#31a354",
        #fillOpacity =,
        smoothFactor = 5
      ) |>
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
        fillOpacity = 0.5,
        popup = ~label
      ) |> 
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
          accident_dead > 0 ~ mpacc_levels$dead,
          accident_serious_injury > 0 ~ mpacc_levels$swound,
          accident_light_injury > 0 ~ mpacc_levels$lwound,
          TRUE ~ mpacc_levels$other
        )
      )
    

    fpal <- colorFactor("Set1", domain = fdata$nasledky, 
                        ordered = TRUE,
                        levels = c(
                          mpacc_levels$dead,
                          mpacc_levels$swound,
                          mpacc_levels$lwound,
                          mpacc_levels$other
                        ))
    
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
        fillOpacity = 0.5,
        popup = ~label
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
      
    #fpal <- colorFactor("Set1", domain = fdata$druh_nehody, levels = levels(fdata$druh_nehody))
    fpal <- colorFactor("Set1", domain = fdata$druh_nehody, 
                        ordered  = TRUE,
                        levels = crashtype_nolinebreaks)
    
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
        title = ACCCHAR$type,
        position = "topright",
        pal = fpal,
        values = fdata$druh_nehody, 
        opacity = 1
      )
    
  })
  
  observeEvent(input$cluster_zoom,{
    
    data_clusters <- get_clusters()

    if(length(input$menu_cluster) == 0){
      selected_cluster <- 1
    }else{
      selected_cluster <- input$menu_cluster
    }

    zoom_clusters <-
      data_clusters$clusters |>
      dplyr::filter(
        cluster == selected_cluster
      ) |>
      dplyr::select(X,Y)

    leafletProxy("mpacc") |>
      setView(
        lng = zoom_clusters$X[1],
        lat = zoom_clusters$Y[1],
        zoom = 16
      )

  })
  
  observeEvent(input$polygon_zoom,{
    
      #if(input$menu_filteraccidents_polygon){
        if(length(input$mpacc_draw_new_feature) != 0){


          coords <-
            get_selectedPOLY() |>
            # sf::st_transform(
            #   sf::st_crs(out)
            # ) |>
            sf::st_geometry() |>
            sf::st_centroid() |>
            sf::st_coordinates() |>
            as.vector()
          
          leafletProxy("mpacc") |>
            setView(
              lng = coords[1],
              lat = coords[2],
              zoom = 16
            )

        }
      #}
    
  })
  
  #### Výběr polygonu na mapě
  
  observeEvent(input$mpacc_draw_new_feature,{
    
    leafletProxy("mpacc") |>
      leaflet::removeMarker("selectedPOLY") |>
      leaflet::addPolygons(
        layerId = "selectedPOLY",
        data = get_selectedPOLY(),
        fill = FALSE
      )
    
  })
  
  
  get_selectedPOLY <- reactive({
    
    if(length(input$mpacc_draw_new_feature) == 0){
      out <- get_map_district()
      return(out)
    }else{
      
    polygon_coordinates <- input$mpacc_draw_new_feature$geometry$coordinates[[1]]
    
    out <- 
      do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})) |>
      sf::st_multipoint() |>
      sf::st_cast("POLYGON") |>
      sf::st_sfc() |>
      sf::st_set_crs(4326)
    
    return(out)
    
    }
  })
  
  #### Reports ####
  
  # output$report_overview <- downloadHandler(
  #   filename = "report_ovr.html",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "report_overview.Rmd")
  #     file.copy("report_overview.Rmd", tempReport, overwrite = TRUE)
  # 
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       district = names(MENU$district)[MENU$district == input$menu_district],
  #       data_accidents_p1 = get_accidents_p1(),
  #       data_accidents_p2 = get_accidents_p2(),
  #       period = input$menu_period,
  #       period_user = input$menu_period2_user,
  #       period2 = input$menu_period2,
  #       accfilter = input$menu_filteraccidents
  #     )
  # 
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  # 
  # output$report_accidents <- downloadHandler(
  #   filename = "report_acc.html",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "report_accidents.Rmd")
  #     file.copy("report_accidents.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       data_accidents = get_accidents_box(),
  #       period = input$menu_period_accidents,
  #       accfilter = input$menu_filteraccidents_accidents
  #     )
  #     
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  # 
  # output$report_cluster <- downloadHandler(
  #   filename = "report_clu.html",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "report_cluster.Rmd")
  #     file.copy("report_cluster.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       data_clusters = get_clusters(),
  #       all_accidents = get_accidents_district(),
  #       cluster_id = input$menu_cluster,
  #       period = input$menu_period_hotspots,
  #       quantile = input$menu_quantile,
  #       spill = input$menu_spill,
  #       sidecar = read_sidecar(
  #         district = input$menu_district,
  #         #profile = input$menu_profile,
  #         profile = first(OPTIONS$profile[OPTIONS$period_menu == input$menu_period_hotspots]),
  #         period_start = first(OPTIONS$period_start[OPTIONS$period_menu == input$menu_period_hotspots]),
  #         period_end = first(OPTIONS$period_end[OPTIONS$period_menu == input$menu_period_hotspots])
  #       ),
  #       sidecardesc = PROFILEDESC
  #       #profile = names(MENU$profile[MENU$profile == input$menu_profile])
  #     )
  #     
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  # 
  # 
  # output$reportset_button <- renderUI({
  #   if(month(Sys.Date()) != 1){
  #     downloadButton("reportset","Pravidelný reporting")
  #   }else{
  #     actionButton("january", "Pravidelný reporting")
  #   }
  # })
  # 
  # observeEvent(input$january, {
  #   # Show a modal when the button is pressed
  #   shinyalert("Chyba!", "Pravidelný reporting není během ledna dustupný.", type = "error")
  # })
  # 
  # output$reportset <- downloadHandler(
  #   filename = "reportset.zip",
  #   content = function(file) {
  #     
  #     rr_today <- Sys.Date()
  #     day(rr_today) <- day(rr_today)
  #     
  #     # if(month(rr_today) == 12){
  #     #   shinyalert("Chyba!", 
  #     #              "Reporty obsahují srovnání nehod, které se staly od 1.1. do konce předcházejícího měsíce. Funkce není dostupná v průběhu ledna.", 
  #     #              type = "error")
  #     #   
  #     #   stop("This is the end.")
  #     # }
  #     
  #     
  #     shinyalert("Pozor!", 
  #                "Generování sady reportů může trvat dlouhou dobu. Reporty obsahují srovnání nehod, které se staly od 1.1. do konce předcházejícího měsíce. Obsah reportu může být zásadně ovlivněn naplněností databáze. Hromadné generování není dostupné v průběhu ledna.", 
  #                type = "warning",
  #                inputId = "wait_reports",
  #                showConfirmButton = FALSE
  #                )
  #     
  #     
  #     # First day of the year
  #     rr_period1_1 <- rr_today
  #     month(rr_period1_1) <- 1
  #     day(rr_period1_1) <- 1
  #     
  #     # Last day of the previous month
  #     rr_period1_2 <- rr_today
  #     day(rr_period1_2) <- 1
  #     day(rr_period1_2) <- day(rr_period1_2) - 1
  #     
  #     # Baseline period (p1)
  #     rr_period1 <- c(rr_period1_1,rr_period1_2)
  #     
  #     # Comparison period (p2)
  #     rr_period2 <- rr_period1
  #     year(rr_period2[1]) <- year(rr_period2[1]) - 1
  #     year(rr_period2[2]) <- year(rr_period2[2]) - 1
  #     
  #     
  #     rr_districts <- 
  #       OPTIONS %>% 
  #       distinct(district) %>% 
  #       pull(district)
  #     
  #     
  #     tmpdir_path <- tempdir()
  #     
  #     for(loop_dist in rr_districts){
  #       
  #       accp1 <- read_rds(
  #         str_c(ACCIDENTS_REPOSITORY,"accidents_",loop_dist,".rds")
  #       ) %>% 
  #         filter(
  #           accident_date >= rr_period1[1],
  #           accident_date <= rr_period1[2]
  #         )
  #       
  #       accp2 <- read_rds(
  #         str_c(ACCIDENTS_REPOSITORY,"accidents_",loop_dist,".rds")
  #       ) %>% 
  #         filter(
  #           accident_date >= rr_period2[1],
  #           accident_date <= rr_period2[2]
  #         )
  #       
  #       params <- list(
  #         district = names(MENU$district)[MENU$district == loop_dist],
  #         data_accidents_p1 = accp1,
  #         data_accidents_p2 = accp2,
  #         period = rr_period1,
  #         period_user = FALSE,
  #         period2 = rr_period2,
  #         accfilter = input$menu_filteraccidents
  #       )
  #       
  #       
  #       tempReport <- file.path(tmpdir_path, "report_overview.Rmd")
  #       file.copy("report_overview.Rmd", tempReport, overwrite = TRUE)
  #       
  #       
  #       rmarkdown::render(tempReport, output_file = file.path(tmpdir_path, str_c(loop_dist,"_overview.html")),
  #                         params = params,
  #                         envir = new.env(parent = globalenv())
  #       )
  #       
  #     }
  #     
  #     closeAlert(num = 0, id = input$wait_reports)
  #     
  #     zip(
  #       zipfile = file,
  #       list.files(path = tmpdir_path, pattern = ".html", full.names = TRUE), 
  #       extras = '-j'
  #     )
  #     
  #   }
  # )
} 




# Run the application 
shinyApp(ui = ui, server = server)
