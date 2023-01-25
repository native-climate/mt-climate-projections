library(tidyverse)
library(magrittr)
library(terra)

dir.create("data",
           showWarnings = FALSE)

t_rast <-
  function(x){
    x %>%
      purrr::map(as.list) %>%
      purrr::transpose() %>%
      purrr::map(terra::rast)
  }

"https://github.com/mt-climate-office/MCA/raw/main/assets/cmip_tasmax.rds" %>%
  readr::read_rds() %>%
  dplyr::filter(model != "ACCESS-CM2") %>%
  dplyr::mutate(diff = list(terra::unwrap(diff))) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    rast =
      list(
        diff %>%
          t_rast() %>%
          purrr::map(mean) %>%
          terra::rast() %>%
          magrittr::set_names(month.abb) %T>%
          terra::writeRaster(
            filename = paste0("data/",scenario[[1]],"-",period[[1]],"-","tasmax.tif"),
            overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"),
            memfrac = 0.9
          )
      )
  )

"https://github.com/mt-climate-office/MCA/raw/main/assets/cmip_freeze-free.rds" %>%
  readr::read_rds() %>%
  dplyr::filter(model != "ACCESS-CM2") %>%
  dplyr::mutate(diff = list(terra::unwrap(diff))) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    rast =
      list(
        diff %>%
          t_rast() %>%
          purrr::map(mean) %>%
          terra::rast() %>%
          magrittr::set_names(month.abb) %T>%
          terra::writeRaster(
            filename = paste0("data/",scenario[[1]],"-",period[[1]],"-","freeze_free.tif"),
            overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"),
            memfrac = 0.9
          )
      )
  )

"https://github.com/mt-climate-office/MCA/raw/main/assets/cmip_pr.rds" %>%
  readr::read_rds() %>%
  dplyr::filter(model != "ACCESS-CM2") %>%
  dplyr::mutate(diff = list(terra::unwrap(diff))) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    rast =
      list(
        diff %>%
          t_rast() %>%
          purrr::map(mean) %>%
          terra::rast() %>%
          magrittr::set_names(month.abb) %T>%
          terra::writeRaster(
            filename = paste0("data/",scenario[[1]],"-",period[[1]],"-","pr.tif"),
            overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"),
            memfrac = 0.9
          )
      )
  )

"https://github.com/mt-climate-office/MCA/raw/main/assets/cmip_con-dry.rds" %>%
  readr::read_rds() %>%
  dplyr::filter(model != "ACCESS-CM2") %>%
  dplyr::mutate(diff = list(terra::unwrap(diff))) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    rast =
      list(
        diff %>%
          terra::rast() %>%
          mean()  %T>%
          terra::writeRaster(
            filename = paste0("data/",scenario[[1]],"-",period[[1]],"-","con_dry.tif"),
            overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"),
            memfrac = 0.9
          )
      )
  )

