rm(list = ls())

create_episode_zones <- function(
    ucdpversion = "latest",
    buffer_percent = 0,
    clipcountry = TRUE
) {
  # Install and load packages needed
  if(!require(tidyverse)) { install.packages("tidyverse"); library(tidyverse) }
  if(!require(lubridate)) { install.packages("lubridate"); library(lubridate) }
  if(!require(sf)) { install.packages("sf"); library(sf) }
  if(!require(cshapes)) { install.packages("cshapes"); library(cshapes) }

  # Directories and file names

  # new: here are the wzones
  zip_path <- system.file("extdata", "wzone.zip", package = "toyacd")
  temp_dir <- tempfile("unzipped_data_")
  dir.create(temp_dir)
  unzip(zip_path, exdir = temp_dir)



  savedir <- file.path("datacreated")
  extdir <- file.path("extdata")
  shapefile_folder <- file.path("extdata", "wzone")
  static_shapefile_name <- "static.shp"
  static_shapefile <- file.path(shapefile_folder, static_shapefile_name)

  # Input validation
  if (!is.numeric(buffer_percent) || buffer_percent < 0 || buffer_percent > 100) {
    stop("Buffer percentage must be a numeric value between 0 and 100.")
  }

  # Initialize lists
  created_polygons <- list()
  static_polygons <- list()

  # Process episodes using yearly shapefiles
  process_episode <- function(episode) {
    conf_id <- as.integer(sub("_.*$", "", episode$conflict_epi_id))
    startyear <- episode$startyear
    endyear <- episode$endyear
    episode_id <- episode$conflict_epi_id

    years_to_load <- seq(startyear, endyear)

    load_and_filter_shapefile <- function(year, conf_id) {
      shapefile_name <- file.path(shapefile_folder, paste0(year, "_12_31.shp"))

      if (file.exists(shapefile_name)) {
        shp <- sf::st_read(shapefile_name, quiet = TRUE)
        if (!is.numeric(shp$conf_id)) shp <- shp %>% mutate(conf_id = as.integer(conf_id))
        shp_filtered <- shp %>% filter(conf_id == !!conf_id)
        shp_filtered <- shp_filtered %>% mutate(geometry = st_make_valid(geometry))
        if (nrow(shp_filtered) > 0) return(shp_filtered)
      }
      return(NULL)
    }

    shapefiles <- purrr::map(years_to_load, ~ load_and_filter_shapefile(.x, conf_id))
    shapefiles_combined <- do.call(rbind, shapefiles[!sapply(shapefiles, is.null)])

    if (is.null(shapefiles_combined) || nrow(shapefiles_combined) == 0) return(NULL)

    shapefiles_combined %>%
      summarise(geometry = st_union(geometry)) %>%
      mutate(geometry = st_make_valid(geometry), confzone_id = episode_id)
  }

  # Process static zones
  process_static_zones <- function() {
    if (file.exists(static_shapefile)) {
      static_shapefile <- sf::st_read(static_shapefile, quiet = TRUE)
      if (!is.numeric(static_shapefile$conf_id)) {
        static_shapefile <- static_shapefile %>% mutate(conf_id = as.integer(conf_id))
      }
      static_polygons <- lapply(unique(static_shapefile$conf_id), function(conf_id) {
        shp_filtered <- static_shapefile %>% filter(conf_id == !!conf_id)
        if (nrow(shp_filtered) > 0) {
          shp_filtered %>%
            summarise(geometry = st_union(geometry)) %>%
            mutate(geometry = st_make_valid(geometry), conf_id = conf_id)
        }
      })
      names(static_polygons) <- unique(static_shapefile$conf_id)
      static_polygons[!sapply(static_polygons, is.null)]
    } else {
      stop("Static shapefile not found.")
    }
  }

  # Load dataset
  files <- list.files(savedir, pattern = "UCDP_conflict_month_basedata_v\\d+\\.rds")
  versions <- as.numeric(stringr::str_extract(files, "\\d+"))
  dataset_version <- ifelse(ucdpversion == "latest", max(versions, na.rm = TRUE), as.numeric(ucdpversion))
  base <- read_rds(file.path(savedir, paste0("UCDP_conflict_month_basedata_v", dataset_version, ".rds")))

  # Prepare episodes list
  episodeslist <- base %>%
    filter(active == 1) %>%
    group_by(conflict_epi_id) %>%
    summarise(startyear = min(year), endyear = max(year), conflict_id = first(conflict_id))

  # Process episodes
  episodeslist$shapefile_created <- FALSE
  for (i in seq_len(nrow(episodeslist))) {
    row <- episodeslist[i, ]
    polygon <- process_episode(list(conflict_epi_id = row$conflict_epi_id, startyear = row$startyear, endyear = row$endyear))
    if (!is.null(polygon) && nrow(polygon) > 0) {
      created_polygons[[row$conflict_epi_id]] <- polygon
      episodeslist$shapefile_created[i] <- TRUE
    }
  }

  # Fill missing zones
  episodeslist <- episodeslist %>%
    arrange(conflict_id, startyear) %>%
    group_by(conflict_id) %>%
    mutate(
      zone_to_use = if_else(
        shapefile_created,
        conflict_epi_id,
        lag(conflict_epi_id, default = NA_character_) %>%
          {if_else(lag(shapefile_created, default = FALSE), ., NA_character_)}
      )
    ) %>%
    ungroup()

  # Process static zones
  static_zones <- process_static_zones()

  # Combine zones
  all_polygons <- purrr::map(seq_len(nrow(episodeslist)), function(i) {
    row <- episodeslist[i, ]
    assigned_zone <- row$zone_to_use
    if (!is.na(assigned_zone) && assigned_zone %in% names(created_polygons)) {
      polygon <- created_polygons[[assigned_zone]]
      source_id <- assigned_zone
    } else {
      static_key <- as.character(row$conflict_id)
      if (static_key %in% names(static_zones)) {
        polygon <- static_zones[[static_key]]
        source_id <- static_key
      } else {
        polygon <- NULL
        source_id <- NA_character_
      }
    }
    if (!is.null(polygon)) {
      polygon %>% mutate(conflict_epi_id = row$conflict_epi_id, source_z = source_id)
    } else {
      NULL
    }
  })

  combined_polygons <- bind_rows(all_polygons[!sapply(all_polygons, is.null)]) %>%
    mutate(geometry = st_make_valid(geometry))

  # Rename usefully
  combined_polygons <- combined_polygons %>%
    select(-c(confzone_id,conf_id)) %>%
    rename(epi_id = conflict_epi_id)

  # Note in base data which episodes have no zones
  zones_nogeom <- combined_polygons %>%
    st_drop_geometry() %>% mutate(haszone = 1)
  base <- base %>%
    left_join(zones_nogeom, by = c("conflict_epi_id" = "epi_id")) %>%
    mutate(missingzone = if_else(is.na(haszone), 1, 0)) %>% select(-c(haszone,source_z))
  write_rds(base, file = file.path(savedir, paste0("UCDP_conflict_month_basedata_v", dataset_version, ".rds")))
  # Also save a copy of this for later linking of violence zones, to keep basedata intact
  write_rds(base, file = file.path(savedir, paste0("UCDP_conflict_month_linked_v", dataset_version, ".rds")))

  # Buffer only if specified
  if (buffer_percent > 0) {
    combined_polygons <- combined_polygons %>%
      mutate(
        original_area = as.numeric(st_area(geometry)),
        target_area = original_area * (1 + buffer_percent / 100),
        scaling_factor = sqrt(target_area / original_area) - 1,
        avg_width = sqrt(original_area) / 2,
        buffer_distance = avg_width * scaling_factor,
        geometry = st_buffer(geometry, dist = buffer_distance) %>% st_make_valid()
      ) %>%
      select(-original_area, -target_area, -scaling_factor, -avg_width, -buffer_distance)
  }

  # Skip clipping if clipcountry == FALSE
  if (!clipcountry) {
    message("Clipping skipped. Saving combined polygons...")
    output_file <- file.path(savedir, paste0("episodezones_v", dataset_version, ".shp"))
    file.remove(list.files(savedir, pattern = paste0("^episodezones_v", dataset_version, "\\..*$"), full.names = TRUE))
    st_write(combined_polygons, output_file, delete_layer = TRUE)
    return(invisible(NULL))
  } else {
    episodesannual <- base %>% filter(active==1) %>% distinct(conflict_epi_id,year, .keep_all = T) %>%  select(conflict_epi_id,conflict_id,gwno_a,year) %>% rename(confcountry = gwno_a)

    # Find out where most events take place (the "event" country)
    ged <- read_rds(file.path(extdir, paste0("ged/ged", dataset_version, ".rds")))
    ged <- ged %>% filter(type_of_violence==1 & active_year==1) %>% select(id,year,conflict_new_id,country_id)
    ged <- left_join(ged,episodesannual, by = c("conflict_new_id"="conflict_id", "year")) %>% filter(!is.na(conflict_epi_id))
    most_events_by_country <- ged %>%
      group_by(conflict_epi_id, country_id) %>%
      summarise(event_count = n(), .groups = "drop") %>%
      group_by(conflict_epi_id) %>%
      slice_max(event_count, n = 1, with_ties = FALSE) %>% rename(eventscountry = country_id)
    relcountrybyepi <- episodesannual %>% distinct(conflict_epi_id, .keep_all = T) %>% left_join(most_events_by_country, by = c("conflict_epi_id"))

    # For successful independence conflicts, change country in which most events took place to where this is NOW
    relcountrybyepi <- relcountrybyepi %>%
      mutate(eventscountry = case_when(
        conflict_id == 298 ~ 565,
        conflict_id == 330 ~ 860,
        conflict_id == 384 ~ 349,
        conflict_id == 385 ~ 344,
        conflict_id == 412 ~ 347,
        conflict_id == 309 ~ 626,
        conflict_id == 376 ~ 373,
        conflict_id == 377 ~ 373,
        conflict_id == 275 ~ 531,
        TRUE ~ eventscountry # Keep existing value if no match
      ))

    # Turn this into one column
    relcountrybyepi <- relcountrybyepi %>%
      mutate(relevantcountries = ifelse(confcountry == eventscountry,
                                        confcountry,
                                        paste(confcountry, eventscountry, sep = ", "))) %>%
      select(-year, -event_count)

    # Attach this to allpolygons
    combined_polygons <- left_join(combined_polygons,relcountrybyepi, by = c("epi_id"="conflict_epi_id"))

    # Load cshapes to have country borders
    cshapes <- cshp(date = NA, useGW = TRUE, dependencies = FALSE) %>%
      filter(end == "2019-12-31")

    cshapes <- cshapes %>%
      mutate(geometry = if_else(st_is_valid(geometry), geometry, st_make_valid(geometry)))

    # Step 1: Function to process each conflict zone
    process_conflict_zone <- function(epi_id, conflict_geometry, relcountrybyepi, cshapes) {
      # Step 1a: Look up relevant GW codes
      relevant_gwcodes <- relcountrybyepi %>%
        filter(conflict_epi_id == epi_id) %>%
        pull(relevantcountries) %>%
        strsplit(",\\s*") %>%
        unlist() %>%
        as.numeric()

      if (length(relevant_gwcodes) == 0) {
        message(paste("No relevant GW codes for epi_id:", epi_id))
        return(NULL)
      }

      # Step 1b: Filter relevant countries from cshapes
      relevant_countries <- cshapes %>%
        filter(gwcode %in% relevant_gwcodes)

      if (nrow(relevant_countries) == 0) {
        message(paste("No matching countries for GW codes for epi_id:", epi_id))
        return(NULL)
      }

      # Step 1c: Make geometries valid
      conflict_geometry <- st_make_valid(conflict_geometry)
      relevant_countries <- st_make_valid(relevant_countries)

      # Step 1d: Perform intersection
      clipped_zone <- tryCatch({
        st_intersection(conflict_geometry, relevant_countries$geometry)
      }, error = function(e) {
        message(paste("Intersection failed for epi_id:", epi_id, "-", e$message))
        return(NULL)
      })

      # Step 1e: Return the result as an sf object
      if (!is.null(clipped_zone) && length(clipped_zone) > 0) {
        return(tibble(epi_id = epi_id, source_z = unique(conflict_geometry$source_z), geometry = st_union(clipped_zone)))
      } else {
        message(paste("No valid intersection for epi_id:", epi_id))
        return(NULL)
      }
    }

    # Step 1: Apply the clipping function rowwise
    clipped_zones_list <- combined_polygons %>%
      rowwise() %>%
      mutate(
        clipped = list(process_conflict_zone(epi_id, geometry, relcountrybyepi, cshapes))
      ) %>%
      ungroup()

    # Step 2: Combine all clipped geometries into a single sf object
    clipped_zones <- do.call(rbind, clipped_zones_list$clipped)

    # Step 3: Ensure it's an sf object
    clipped_zones <- st_as_sf(clipped_zones)

    # Save clipped zones to the same file name
    message("Saving clipped conflict zones...")
    output_file <- file.path(savedir, paste0("episodezones_v", dataset_version, ".shp"))
    file.remove(list.files(savedir, pattern = paste0("^episodezones_v", dataset_version, "\\..*$"), full.names = TRUE))
    st_write(clipped_zones, output_file, delete_layer = TRUE)
    message("Clipped conflict zones saved successfully.")
  }
}
