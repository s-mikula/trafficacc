# Load libraries, functions, and pre-defied variables
source("functions.R")

OPTIONS <- list_options()

PERIOD_preselected <- OPTIONS |> 
  dplyr::slice_max(period_end, n = 1L) |>
  dplyr::slice(1L) |>
  dplyr::pull(period_menu)

map_districts <- 
  readr::read_rds(
    stringr::str_c(APPDATA_REPOSITORY,"districts.rds")
  ) %>% 
  sf::st_transform(4326)

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
      menuItem("Dopravní nehody", tabName = "accidents", icon = icon("car-burst"))
    ),
    sidebarMenu(
      selectInput(
        "menu_district", "Okres:",
        MENU$district[MENU$district %in% OPTIONS$district],
        selected = "CZ0642",
        multiple = FALSE,
        width = '100%'
      ),
      img(src='muni-lg-white.png', align = "center", width = "100%"),br(),
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
                column(3,
                       box(
                         dateRangeInput(
                           "menu_period",
                           "Základní období (od/do):",
                           start = str_c(year(today())-1,"-01-01"),
                           end = str_c(year(today())-1,"-12-31"),
                           min = MINIMUM_DATE,
                           max = str_c(year(today()),"-12-31"),
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = " do ",
                           width = '100%'
                         ),
                         materialSwitch(
                           "menu_period2_user",
                           "Uživatelem nastavené srovnávací období",
                           inline = TRUE,
                           value = FALSE,
                           width = '100%'
                         ),
                         dateRangeInput(
                           "menu_period2",
                           label = NULL,
                           start = max(OPTIONS$period_start),
                           end = max(OPTIONS$period_end),
                           min = MINIMUM_DATE,
                           max = max(OPTIONS$period_end),
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = " do ",
                           width = '100%'
                         ),
                         actionLink("help_periodovr","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         selectInput(
                           "menu_filteraccidents",
                           "Výběr dopravních nehod:",
                           MENU$filteraccidents,
                           selected = "all",
                           width = '100%'
                         ),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         p("Vygeneruje tisknutelný report v HTML."),
                         downloadButton("report_overview", "Report"),
                         status = "warning",
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
                         title = "Počet nehod podle věku řidiče (viníka nehody)",
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
              ),
              fluidRow(
                column(12,
                       box(
                         "Vývoj webové aplikace a software na identifikaci klastrů nehod na silniční síti byl podpořen
                         grantem TA ČR (CK01000049): ",
                         em("Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR"),br(),
                         "Autoři: Michal Kvasnička (michal.kvasnicka@econ.muni.cz) & Štěpán Mikula (stepan.mikula@econ.muni.cz)",
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
                         actionLink("help_period_clusters","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         selectInput(
                           "menu_profile",
                           "Profil:",
                           MENU$profile[MENU$profile %in% OPTIONS$profile],
                           selected = "default",
                           width = '100%'
                         ),
                         actionLink("help_profile","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         sliderInput(
                           "menu_quantile",
                           "Promile nejhorších úseků:",
                           min = 1,
                           max = 25,
                           step = 2,
                           value = 5,
                           ticks = TRUE,
                           width = '100%'
                         ),
                         actionLink("help_severity","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         sliderInput(
                           "menu_spill",
                           "Dodatečný rozliv klastrů:",
                           min = 1,
                           max = 10,
                           value = 2,
                           step = 1,
                           ticks = TRUE,
                           width = '100%'
                         ),
                         actionLink("help_spill","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(3,
                       box(
                         p("Generování HTML reportu pro velké množství nehod může trvat dlouhou dobu."),
                         downloadButton("report_cluster", "Report"),
                         downloadButton("downloadPOLY", "GeoJSON"),
                         status = "warning",
                         width = 12
                       )
                       )
              ),
      fluidRow(
        column(6,
               box(
               leafletOutput("mphot", height = 900),
               title = "Mapa shluků dopravních nehod",
               width = 12
               )
               ),
        column(6,
               box(
                 selectInput(
                   "menu_sorting",
                   "Řazení klastrů",
                   choices = MENU$sorting,
                   selected = "costs",
                   width = '100%'
                 ),
                 plotOutput(
                   "fig_clusters",
                   height = "250px"
                   ),
                 uiOutput("controlSorting"),
                 actionLink("help_sorting","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                 status = "warning",
                 width = 12
               ),
               valueBoxOutput("box_acc_n_hot", width = 3),
               valueBoxOutput("box_death_n_hot", width = 3),
               valueBoxOutput("box_swound_n_hot", width = 3),
               valueBoxOutput("box_lwound_n_hot", width = 3),
               box(
                 tableOutput("tab_casualties_hot"), 
                 title = "Oběti dopravních nehod",
                 width = 12
               ),
               box(
                 tableOutput("tab_fault_hot"), #tab_causes_accidents
                 title = "Nehody podle zavinění",
                 width = 12
               ),
               box(
                 tableOutput("tab_causes_hot"), #tab_type_accidents
                 title = "Nehody podle příčin",
                 width = 12
               ),
               box(
                 tableOutput("tab_type_hot"), #tab_type_accidents
                 title = "Nehody podle příčin",
                 width = 12
               ),
               box(
                 plotOutput("fig_age_hot"),
                 title = "Počet nehod podle věku řidiče",
                 width = 12
               )
        )
        ),
      fluidRow(
        column(12,
               box(
                 "Vývoj webové aplikace a software na identifikaci klastrů nehod na silniční síti byl podpořen
                         grantem TA ČR (CK01000049): ",
                 em("Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR"),br(),
                 "Autoři: Michal Kvasnička (michal.kvasnicka@econ.muni.cz) & Štěpán Mikula (stepan.mikula@econ.muni.cz)",
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
                           "Období (od/do):",
                           start = str_c(year(today())-1,"-01-01"),
                           end = str_c(year(today())-1,"-12-31"),
                           min = MINIMUM_DATE,
                           max = str_c(year(today()),"-12-31"),
                           format = "dd.mm.yyyy",
                           language = "cs",
                           separator = " do ",
                           width = '100%'
                         ),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         selectInput(
                           "menu_filteraccidents_accidents",
                           "Výběr dopravních nehod:",
                           MENU$filteraccidents,
                           selected = "all",
                           width = '100%'
                         ),
                         #actionLink("help_filteraccidents","Nápověda",icon = icon("circle-info", lib="font-awesome")),
                         status = "warning",
                         width = 12
                       )
                ),
                column(2,
                       box(
                         "Omezit na nehody v oblasti poslední vybrané nehodové lokality.",
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
                column(2,
                       box(
                         "Omezit nehody výběrem polygonu na mapě.",
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
                ),
                # column(3,
                #        box(
                #          p("Výběr polygonu na mapě omezuje výběr dopravních nehod na danou oblast. 
                #               Ostatní nastavené filtry zůstavájí v platnosti (AND)."),
                #          actionButton("removefilter", "Filtr není aktivní"),
                #          #title = "Výběr oblasti",
                #          status = "warning",
                #          width = 12
                #        )
                # ),
                column(3,
                       box(
                         p("Generování HTML reportu pro velké množství nehod může trvat dlouhou dobu."),
                         downloadButton("report_accidents", "Report"),
                         downloadButton("downloadID", "ID nehod"),
                         status = "warning",
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
                            radioButtons("mpacc_legend", 
                                         label = "Zbarvení dopravních nehod",
                            choices = list(
                              "Podle následků" = "nasledky", 
                              "Podle druhu nehody" = "druh_nehody"
                            ), 
                            selected = "nasledky"),
                            status = "warning",
                            width = 6
                            ),
                      box(
                        "Zelený polygon na mapě odpovídá oblasti poslední vybrané nehodové lokality",
                        "na tabu 'Nehodové lokality'. Uživatel může pomocí mapových nástrojů nakreslit vlastní polygon, který je zobrazen modrou barvou. ",
                        "Nehody lze podle obou polygonů filtrovat."
                      )
                      # box(
                      #   "Vyhledání poslední vybrané nehodové lokality",
                      #   actionButton(
                      #     "cluster_zoom", 
                      #     label = "Zoom"),
                      #   width = 6
                      # )
                        ),
              column(5,
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
          ),
          fluidRow(
            column(12,
                   box(
                     "Vývoj webové aplikace a software na identifikaci klastrů nehod na silniční síti byl podpořen
                         grantem TA ČR (CK01000049): ",
                     em("Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR"),br(),
                     "Autoři: Michal Kvasnička (michal.kvasnicka@econ.muni.cz) & Štěpán Mikula (stepan.mikula@econ.muni.cz)",
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
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = input$click, { 
    showModal(modalDialog(
      title = "Aplikace pro interaktivní analýzu dopravních nehod", 
      p("Toto je ukázková verze aplikace, která byla vyvinuta pro potřeby Policie ČR v rámci projektu 'TA ČR (CK01000049): Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR'."),
      p("Aplikace je primárně určena pro dopravní experty, kteří dokážou údaje správně interpretovat. Ukázková verze aplikace obsahuje data pouze pro vybrané okresy ČR a využívá pouze volně dostupná data."),
      h4("Co aplikace umí?"),
      p("Aplikace nabízí tři pohledy na data. Na tabu 'Přehled' vidí uživatel souhrnné statistiky pro vybraný okres a období."),
      p("Tab 'Nehodové lokality' ukazuje identifikované nehodové lokality a statistiky pro uživatelem vybranou nehodovou lokalitu (klastr). Nehodové lokality jsou identifikovány pomocí inovativní technologie, která umožňuje analýzu nehod na celé silniční síti."),
      p("Poslední tab 'Dopravní nehody' dává uživateli největší volnost. Může pracovat se statistikami nehod z libovolného úseku dopravní sítě a libovolného období. But with a great power comes great responsibility."),
      h4("Poznámka"),
      p("V základním nastavení aplikace zobrazuje okres Brno-město. It's not a bug. It's a feature."),
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
    shinyalert("Nápověda", HELP$spill_text, type = "info")
  })
  
  observeEvent(input$help_period_clusters, {
    shinyalert("Nápověda", HELP$period_clusters_text, type = "info")
  })
  
  observeEvent(input$help_profile, {
    shinyalert("Nápověda", HELP$profile_text, type = "info")
  })
  
  observeEvent(input$help_severity, {
    shinyalert("Nápověda", HELP$severity_text, type = "info")
  })
  
  observeEvent(input$help_sorting, {
    shinyalert("Nápověda", HELP$sorting_text, type = "info")
  })
  
  observeEvent(input$help_filtercluster, {
    shinyalert("Nápověda", HELP$filtercluster_text, type = "info")
  })
  
  observeEvent(input$help_filterpolygon, {
    shinyalert("Nápověda", HELP$filterpolygon_text, type = "info")
  })
  
  observeEvent(input$help_periodovr, {
    shinyalert("Nápověda", HELP$periodovr_text, type = "info")
  })
  
  
  
  ##### Header #####
  
  output$header_title <- renderText({
    stringr::str_c("Nehody v okrese ",
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
        title = "Konec základního období předchází jeho začátku",
        type = "warning",
        text = "Specifikace období není korektní. Aplikace vyměnila začátek/konec období."
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
          title = "Konec srovnávacího období předchází jeho začátku",
          type = "warning",
          text = "Specifikace období není korektní. Aplikace vyměnila začátek/konec období."
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
        title = "Rozdílná délka základního a srovnávacího období",
        type = "warning",
        text = "Délka základního a srovnávacího období se liší. Statistiky srovnávající obě období, které mají nyní různou délku, tak nebudou vypovídající."
        )
    }
    
    if(p2_end >= p1_start & p2_end <= p1_end){
      shinyalert::shinyalert(
        title = "Základní a srovnávací období se překrývají",
        type = "warning",
        text = "Základní a srovnávací období se překrývají. Statistiky srovnávající obě období, které mají nyní různou délku, tak nebudou vypovídající."
      )
    }
    
    if(p2_start >= p1_end){
      shinyalert::shinyalert(
        title = "Základní období předchází srovnávacímu období",
        type = "warning",
        text = "Základní období předchází srovnávacímu období. Věnujte pozornost správné interpretaci statistik, které srovnávající obě období."
      )
    }
    
    
    if(p2_start < MINIMUM_DATE){
      shinyalert::shinyalert(
        title = "Rozdílná délka základního a srovnávacího období",
        type = "warning",
        text = stringr::str_c("Databáze obsahuje nehody, které se staly od 1.1.2011. ",
                              "Při vybraném základním období by srovnávací období (",
                              p2_string,") sahalo před toto datum. Statistiky srovnávající obě období, které mají nyní různou délku, tak nebudou vypovídající."
        )
      )
      
      p2_start <- MINIMUM_DATE
      
      p2_string <- stringr::str_c(
        strftime(p2_start, format = "%d.%m.%Y"),
        " - ",
        strftime(p2_end, format = "%d.%m.%Y")
      )
    }
    
    
    stringr::str_c("Srovnávací období: ",
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
    
    out <- out |>
      dplyr::mutate(
        label = str_c(
          "<b>ID nehody: </b>", accident_id,"<br>",
          "<b>Datum nehody: </b>", strftime(accident_date, format = "%d.%m.%Y"),"<br>",
          "<b>Druh nehody: </b>", crashtype_nolinebreaks[as.character(accident_type)],"<br>",
          "<b>Počet mrtvých: </b>", accident_dead,"<br>",
          "<b>Počet těžce zraněných: </b>", accident_serious_injury,"<br>",
          "<b>Počet lehce zraněných: </b>", accident_light_injury,"<br>",
          "<b>Škoda na majetku: </b>", format(1e6*accident_material_cost, nsmall=0, trim=TRUE, big.mark=" ")
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
          "<b>ID nehody: </b>", accident_id,"<br>",
          "<b>Datum nehody: </b>", strftime(accident_date, format = "%d.%m.%Y"),"<br>",
          "<b>Počet mrtvých: </b>", accident_dead,"<br>",
          "<b>Počet těžce zraněných: </b>", accident_serious_injury,"<br>",
          "<b>Počet lehce zraněných: </b>", accident_light_injury,"<br>",
          "<b>Škoda na majetku: </b>", format(1e6*accident_material_cost, nsmall=0, trim=TRUE, big.mark=" ")
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
      )
    
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
      
      
      # polygon_coordinates <- input$mpacc_draw_new_feature$geometry$coordinates[[1]]
      # 
      # map_selection_polygon <- 
      #   do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})) |>
      #   sf::st_multipoint() |>
      #   sf::st_cast("POLYGON") |>
      #   sf::st_sfc() |>
      #   sf::st_set_crs(4326) |>
      #   sf::st_transform(
      #     crs = sf::st_crs(out)
      #   )
      
      # sfpoly_centroid <- 
      #   get_selectedPOLY() |>
      #   sf::st_transform(
      #     sf::st_crs(out)
      #   ) |>
      #   sf::st_geometry() |>
      #   sf::st_centroid() |>
      #   sf::st_geometry() |>
      #   as.vector()
      # 
      # leafletProxy("mpacc") |>
      #   setView(
      #     lng = sfpoly_centroid[1],
      #     lat = sfpoly_centroid[2],
      #     zoom = 16
      #   )
      
      #if(length(input$map_selection) != 0){
      #if(input$map_selection == "yes"){
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
    
    # if(length(input$map_polygon) > 1){
    #   map_selection <- 
    #     input$map_polygon |>
    #     as.double() |>
    #     matrix(ncol = 2) |>
    #     as.data.frame() |> 
    #     sf::st_as_sf(
    #       coords = c(1,2),
    #       crs = 4326
    #     ) %>% 
    #     sf::st_geometry() |>
    #     sf::st_union() |> 
    #     sf::st_cast("POLYGON") |>
    #     sf::st_transform(
    #       crs = sf::st_crs(out)
    #     )
    # }else{
    #   map_selection <- 
    #     get_accidents_district() |>
    #     sf::st_transform(
    #       crs = sf::st_crs(out)
    #     )
    # }
    
    
      #if(input$map_polygon[1] != "none"){

        # out <- 
        #   sf::st_filter(
        #     x = out,
        #     y = get_selectedPOLY()
        #     )

        # out <- out |>
        #   sf::st_filter(selected_polygon)
        #   # sf::st_intersection(
        #   #   out, selected_polygon
        #   # )
      #}
    
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
  
  ###### Read clusters #####
  get_clusters <- reactive({
    
    out <- read_clusters(
      district = input$menu_district,
      profile = input$menu_profile,
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
  
  output$controlSorting <- renderUI({
    
    maxN <- get_clusters()
    
    tagList(
      # selectInput("menu_cluster", 
      #             "Výběr klastru dopravních nehod", 
      #             choices = 1:maxN$N, 
      #             selected = 1,
      #             width = '100%'
      #             )
      sliderInput("menu_cluster", 
                  "Výběr klastru dopravních nehod", 
                  min = 1,
                  max = maxN$N,
                  step = 1,
                  value = 1,
                  width = '100%'
      )
    )
  })
  
  ###### Boxes #####
  
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
      pr, "Dopravních nehod", 
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
      pr, "Usmrceno osob", 
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
      pr, "Těžce zraněno osob", 
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
      pr, "Lehce zraněno osob", 
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
        "Závažnost klastru\nnehod"
      ) +
      scale_x_continuous(
        "Pořadí klastru nehod podle závažnosti"
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
  
  ###### IDs #####
  
  # output$downloadIDcl <- downloadHandler(
  #   filename = "accidentsIDcl.csv",
  #   content = function(file) {
  #     
  #     get_accidents_box() |>
  #       dplyr::select(p1 = accident_id) |>
  #       sf::st_drop_geometry() |>
  #       readr::write_csv(file = file)
  #     
  #   }
  # )
  
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
  
  #session$sendCustomMessage("map_polygon", map_selection_polygon_vec)
  
  ###### Maps ######
  
  output$mphot <- renderLeaflet({
    
    data_clusters <- get_clusters()
    
    clusters_accidents <- get_accidents_district() |>
      dplyr::filter(
        accident_id %in% data_clusters$accidents$accident_id
      ) |>
      dplyr::mutate(
        label = str_c(
          "<b>ID nehody: </b>", accident_id,"<br>",
          "<b>Datum nehody: </b>", strftime(accident_date, format = "%d.%m.%Y"),"<br>",
          "<b>Druh nehody: </b>", crashtype_nolinebreaks[as.character(accident_type)],"<br>",
          "<b>Počet mrtvých: </b>", accident_dead,"<br>",
          "<b>Počet těžce zraněných: </b>", accident_serious_injury,"<br>",
          "<b>Počet lehce zraněných: </b>", accident_light_injury,"<br>",
          "<b>Škoda na majetku: </b>", format(1e6*accident_material_cost, nsmall=0, trim=TRUE, big.mark=" ")
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
          "Ve výběru nejsou žádné smrtelné nehody.",""
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
          "Ve výběru nejsou žádné zaviněné nehody.",""
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
          "Ve výběru nejsou žádné nehody se známou příčinou.",""
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
          "Ve výběru nejsou žádné nehody se známou příčinou.",""
        ))
      }
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
  
  # get_zoom <- reactive({
  #   out <- list()
  #   out$zoom_view <- FALSE
  #   out$coords <- c(0,0)
  # 
  #   if(input$menu_filteraccidents_polygon){
  #     if(length(input$mpacc_draw_new_feature) != 0){
  # 
  # 
  #       out$coords <-
  #         get_selectedPOLY() |>
  #         # sf::st_transform(
  #         #   sf::st_crs(out)
  #         # ) |>
  #         sf::st_geometry() |>
  #         sf::st_centroid() |>
  #         sf::st_coordinates() |>
  #         as.vector()
  # 
  #       out$zoom_view <- TRUE
  # 
  #     }
  #   }
  # 
  #   return(out)
  # })
  
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
    
    #ZOOM <- get_zoom()
    
    #fpal <- colorFactor("Set1", domain = fdata$nasledky, levels = levels(fdata$nasledky))
    fpal <- leaflet::colorFactor("Set1", 
                                 domain = fdata$nasledky,
                                 ordered = TRUE,
                        levels = c(
                          "Nehoda s obětí na životech",
                          "Nehoda s těžkým zraněním",
                          "Nehoda s lehkým zraněním",
                          "Ostatní nehody"
                        ))
    
    leaflet(data = get_map_district()) |>
      #leaflet(data = select_()) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") |>
      addTiles(group = "OSM (default)") |>
      addProviderTiles('Esri.WorldImagery', group = "Satelite") |>
      addPolygons(
        fill = FALSE
      ) %>% 
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
      ) #%>% 
      # {
      #   ifelse(
      #     ZOOM$zoom_view,
      #     . |>
      #       setView(
      #         lng = out$coords[1],
      #         lat = out$coords[2],
      #         zoom = 16
      #       ),
      #     .
      #   )
      # }
      
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
    
    #fpal <- colorFactor("Set1", domain = fdata$nasledky, levels = levels(fdata$nasledky))
    fpal <- colorFactor("Set1", domain = fdata$nasledky, 
                        ordered = TRUE,
                        levels = c(
                          "Nehoda s obětí na životech",
                          "Nehoda s těžkým zraněním",
                          "Nehoda s lehkým zraněním",
                          "Ostatní nehody"
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
        title = "Druh nehody",
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
  
  # # https://redoakstrategic.com/geoshaper/
  # observeEvent(input$mpacc_draw_new_feature,{
  #   polygon_coordinates <- input$mpacc_draw_new_feature$geometry$coordinates[[1]]
  #   
  #   
  #   # Accidents
  #   sf_accidents <- get_accidents()
  #   
  #   
  #   # Create sf polygon
  #   map_selection_polygon <- 
  #     do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})) |>
  #     sf::st_multipoint() |>
  #     sf::st_cast("POLYGON") |>
  #     sf::st_sfc() |>
  #     sf::st_set_crs(4326) |>
  #     sf::st_transform(
  #       crs = sf::st_crs(sf_accidents)
  #     )
  # 
  #   map_selection_vec <-
  #     map_selection_polygon |>
  #     sf::st_intersection(sf_accidents,y = _) |>
  #     dplyr::pull(accident_id) |>
  #     as.character()
  #   
  #   map_selection_polygon_vec <- 
  #     # do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})) |> 
  #     # as.character() |>
  #     # as.vector()
  #     map_selection_polygon |>
  #     sf::st_as_sf() |>
  #     geojsonsf::sf_geojson()
  #   
  #   session$sendCustomMessage("map_polygon", map_selection_polygon_vec)
  #   session$sendCustomMessage("map_selection", map_selection_vec)
  #   
  #   leafletProxy("mpacc") |>
  #     clearGroup("draw_selection") |> 
  #     clearGroup("draw") |>
  #     addPolygons(
  #       data = map_selection_polygon,
  #       color = "#ff0066",
  #       group = "draw_selection"
  #     )
  #   
  #   #write_lines(map_selection_polygon_vec, file = "aux.txt")
  #   
  #   updateActionButton(session, "removefilter", label = "Smazat filtr")
  # })
  
  observeEvent(input$mpacc_draw_new_feature,{
    
    leafletProxy("mpacc") |>
      leaflet::removeMarker("selectedPOLY") |>
      leaflet::addPolygons(
        layerId = "selectedPOLY",
        data = get_selectedPOLY(),
        fill = FALSE
      )
    
    #session$sendCustomMessage("map_selection", "yes")
    #updateActionButton(session, "removefilter", label = "Smazat filtr")
  })
  
  # observeEvent(input$menu_filteraccidents_polygon,{
  #   if(!input$menu_filteraccidents_polygon){
  #   leafletProxy("mpacc") |>
  #     leaflet::removeMarker("selectedPOLY")
  #   }
  # 
  #   #session$sendCustomMessage("map_selection", "no")
  #   #updateActionButton(session, "removefilter", label = "Filtr není aktivní")
  # })
  
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
  
  output$report_overview <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_ovr.html",
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
        period_user = input$menu_period2_user,
        period2 = input$menu_period2,
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
  
  output$report_accidents <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_acc.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_accidents.Rmd")
      file.copy("report_accidents.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        data_accidents = get_accidents_box(),
        period = input$menu_period_accidents,
        accfilter = input$menu_filteraccidents_accidents
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
  
  output$report_cluster <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_clu.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_cluster.Rmd")
      file.copy("report_cluster.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        data_clusters = get_clusters(),
        all_accidents = get_accidents_district(),
        cluster_id = input$menu_cluster,
        period = input$menu_period_hotspots,
        quantile = input$menu_quantile,
        spill = input$menu_spill,
        profile = names(MENU$profile[MENU$profile == input$menu_profile])
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
