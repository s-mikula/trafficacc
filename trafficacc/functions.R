#### Libraries ####
library(dtplyr, quietly = TRUE)
library(tibble, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(purrr, quietly = TRUE)
library(stringr, quietly = TRUE)
library(sf, quietly = TRUE)
library(fs, quietly = TRUE)
library(readr, quietly = TRUE)
library(s2, quietly = TRUE)

library(shiny, quietly = TRUE)
library(shinydashboard, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
library(shinyalert, quietly = TRUE)

library(RColorBrewer, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(leaflet.extras, quietly = TRUE)

library(ggplot2, quietly = TRUE)
library(ggside, quietly = TRUE)
#library(ggvis, quietly = TRUE)
library(rmarkdown, quietly = TRUE)
library(OpenStreetMap, quietly = TRUE)
#library(geojsonsf, quietly = TRUE)


#### Constants/settings #####

# Minimum date in the period selection
MINIMUM_DATE <- as_date("2011-01-01")

# Path to a directory with data files on accidents
ACCIDENTS_REPOSITORY <- "accidents/"

# Path to a directory with data files on accidents
CLUSTERS_REPOSITORY <- "clusters/"

# Path to a directory with auxiliary data files (maps etc.)
APPDATA_REPOSITORY <- "districts/"


#### Functions #####
# New

#
# Functionality: 
# Searches data repository and returns all available pre-calculated combinations of 
# districts, profiles, and periods
#
# Inputs: ---
# Output: Tibble with columns district, profile, period_start, period_end
#

# list_options <- function(){
#   fs::dir_ls(
#     CLUSTERS_REPOSITORY
#   ) |>
#     stringr::str_subset(
#       stringr::str_c(CLUSTERS_REPOSITORY,"clusters")
#     ) |>
#     stringr::str_remove_all(
#       CLUSTERS_REPOSITORY
#     ) |>
#     stringr::str_remove_all(".rds") |>
#     stringr::str_remove_all("clusters_") |>
#     tibble::tibble(
#       file = _
#     ) |>
#     tidyr::separate(
#       file, 
#       c("district","profile","period_start","period_end"),
#       sep = "_"
#     ) %>% 
#     dplyr::mutate(
#       period_start = lubridate::as_date(period_start),
#       period_end = lubridate::as_date(period_end),
#       period_menu = stringr::str_c(
#         base::strftime(period_start, "%d.%m.%Y"),
#         " - ",
#         base::strftime(period_end, "%d.%m.%Y")
#       )
#     )
# }

list_options <- function(){
  clusters_available <- fs::dir_ls(CLUSTERS_REPOSITORY, 
                                   regexp="sidecar") |>
    purrr::map_dfr(read_rds) |>
    filter(
      fs::file_exists(
        stringr::str_c(CLUSTERS_REPOSITORY,file_name)
      )
    ) %>% 
    rowwise() %>% 
    dplyr::mutate(
      hash_control = openssl::md5(file(file.path(
        stringr::str_remove(CLUSTERS_REPOSITORY,"/"),
        file_name
        ))) %>% list()
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      hash = hash |> map_chr(as.character),
      hash_control = hash_control |> map_chr(as.character)
    ) |>
    dplyr::filter(
      hash == hash_control
    )
  
  clusters_available |> 
    dplyr::select(
      district = district_id,
      profile = PROFILE_NAME,
      period_start = from_date,
      period_end = to_date,
      PROFILE_COMMENT
    ) |>
    rowwise() |>
    dplyr::mutate(
      period_menu = stringr::str_c(
        base::strftime(period_start, "%d.%m.%Y"),
        " - ",
        base::strftime(period_end, "%d.%m.%Y"),
        " (",
        PROFILE_COMMENT,
        ")"
      )
    ) |>
    ungroup()
}

#
# Functionality: Reads pre-calculated data from an rds file stored in data repository.
#
# Inputs: 
# district...district ID,
# profile...profile name,
# period_start...date in "%Y-%m-%d" format
# period_end...date in "%Y-%m-%d" format
#
# Output: rds file content
#
# Note: The function is design to work with parameters delivered by list_options()
# 

read_clusters <- function(district = NULL, profile = "cost", period_start = NULL, period_end = NULL){
  rdsfile <- stringr::str_c(
    CLUSTERS_REPOSITORY,
    "clusters_",
    district,"_",
    profile,"_",
    as.character(period_start),"_",
    as.character(period_end),
    ".rds"
  )
  
  read_rds(rdsfile)
}

read_sidecar <- function(district = NULL, profile = "cost", period_start = NULL, period_end = NULL){
  rdsfile <- stringr::str_c(
    CLUSTERS_REPOSITORY,
    "clusters_",
    district,"_",
    profile,"_",
    as.character(period_start),"_",
    as.character(period_end),
    "_sidecar.rds"
  )
  
  read_rds(rdsfile)
}

#
# Functionality: Reads data on accidents from an rds file stored in data repository.
#
# Inputs: 
# district...district ID,
#
# Output: rds file content (tibble)
#
# Note: The function is design to work with parameters delivered by list_options()
# 

read_accidents <- function(district = NULL){
  rdsfile <- stringr::str_c(
    ACCIDENTS_REPOSITORY,
    "accidents_",
    district,
    ".rds"
  )
  
  read_rds(rdsfile)
}

#
# Functionality: Returns code for arrow symbol 
#
# Input: number
# Output: Arrow oriented by the number (positive, zero, negative)
#

get_arrow <- function(x){
  if(x > 0){
    return("arrow-up")
  }else if(x == 0){
    return("arrow-right")
  }else{
    return("arrow-down")
  }
}

get_delta <- function(p1,p2){
  
  ch <- ifelse((p1-p2)>0, str_c("+",p1-p2), p1-p2)
  chp <- (100*(p1-p2)/p2) |> 
    round(digits = 1) |>
    format(nsmall = 1, trim = TRUE, big.mark = " ") |> 
    stringr::str_c(" %")
  chp <- ifelse((p1-p2)>0, str_c("+",chp), chp)
  
  nch <- p1-p2 
  nch <- nch |> 
    format(nsmall = 1, trim = TRUE, big.mark = " ")
  
  if(p1 != p2){
    nch <- ifelse(p1-p2, str_c("",nch), str_c("+",nch))
  }
  
  str_c(nch," (",chp,")")
}

conditional_view <- function(lfl, condition = FALSE, centroid = NULL, lflzoom = 16){
  if(condition){
    out <- lfl |>
      setView(
                lng = centroid[1],
                lat = centroid[2],
                zoom = lflzoom
              )
    
    return(out)
  }else{
    return(lfl)
  }
}

remove_linebreaks <- function(x){
  
  if(!is.list(x)){
    x %>%
      as.list() |>
      purrr::map_chr(
        stringr::str_replace_all,
        "\\n"," "
      )
  }else{
    x %>%
      as.list() |>
      purrr::map_chr(
        stringr::str_replace_all,
        "\\n"," "
      )
  }
  
}