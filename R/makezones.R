#' Creates a conflict zone (spatial object) for each episode present in the user-created conflict-month dataset
#'
#' @param basedata Passes the conflict-month dataset on
#' @param buffer_percent How much buffer to add around each conflict zone (in percent, numeric from 0 to 100). Default is no buffer.
#' @param clipcountry Whether to clip conflict zones at the borders of the country/countries primarily affected by the conflict. Default is yes.
#'
#' @return The spatial objects, and the conflict-month data with a column added on whether there is a zone available for that episode
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lubridate
#' @import sf
#' @import cshapes
#'
#' @examples
#' \dontrun{
#'   makezones(basedata, clipcountry = FALSE, buffer_percent = 20)
#' }
#' \dontrun{
#'   makezones(basedata)
#' }
#' \dontrun{
#'   makezones(basedata, clipcountry = TRUE)
#' }
makezones <- function(
    basedata,
    buffer_percent = 0,
    clipcountry = TRUE
) {

  # Locate and unzip the wzone shapefiles
  wzone_zip <- system.file("extdata", "wzone.zip", package = "toyacd")
  if (wzone_zip == "") {
    stop("wzone.zip not found in the package's extdata folder.")
  }
  tmp_dir <- tempdir()
  unzip(wzone_zip, exdir = tmp_dir)

  shapefile_folder <- file.path(tmp_dir, "wzone")
  static_shapefile_name <- "static.shp"
  static_shapefile <- file.path(shapefile_folder, static_shapefile_name)

  # Input validation
  if (!is.numeric(buffer_percent) || buffer_percent < 0 || buffer_percent > 100) {
    stop("Buffer percentage must be a numeric value between 0 and 100.")
  }

  # Initialize lists
  created_polygons <- list()
  static_polygons <- list()

  # Process episode function
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
        if (!is.numeric(shp$conf_id)) shp <- shp %>% dplyr::mutate(conf_id = as.integer(conf_id))
        shp_filtered <- shp %>% dplyr::filter(conf_id == !!conf_id)
        shp_filtered <- shp_filtered %>% dplyr::mutate(geometry = sf::st_make_valid(geometry))
        if (nrow(shp_filtered) > 0) return(shp_filtered)
      }
      return(NULL)
    }

    shapefiles <- purrr::map(years_to_load, ~ load_and_filter_shapefile(.x, conf_id))
    shapefiles_combined <- do.call(rbind, shapefiles[!sapply(shapefiles, is.null)])
    if (is.null(shapefiles_combined) || nrow(shapefiles_combined) == 0) return(NULL)
    shapefiles_combined %>%
      dplyr::summarise(geometry = sf::st_union(geometry)) %>%
      dplyr::mutate(geometry = sf::st_make_valid(geometry), confzone_id = episode_id)
  }

  process_static_zones <- function() {
    if (file.exists(static_shapefile)) {
      static_sf <- sf::st_read(static_shapefile, quiet = TRUE)
      if (!is.numeric(static_sf$conf_id)) {
        static_sf <- static_sf %>% dplyr::mutate(conf_id = as.integer(conf_id))
      }
      static_polygons <- lapply(unique(static_sf$conf_id), function(conf_id) {
        shp_filtered <- static_sf %>% dplyr::filter(conf_id == !!conf_id)
        if (nrow(shp_filtered) > 0) {
          shp_filtered %>%
            dplyr::summarise(geometry = sf::st_union(geometry)) %>%
            dplyr::mutate(geometry = sf::st_make_valid(geometry), conf_id = conf_id)
        }
      })
      names(static_polygons) <- unique(static_sf$conf_id)
      static_polygons[!sapply(static_polygons, is.null)]
    } else {
      stop("Static shapefile not found.")
    }
  }

  # Prepare episodes list
  episodeslist <- basedata %>%
    dplyr::filter(active == 1) %>%
    dplyr::group_by(conflict_epi_id) %>%
    dplyr::summarise(startyear = min(year),
                     endyear = max(year),
                     conflict_id = dplyr::first(conflict_id),
                     .groups = "drop")

  # Process episodes
  message("Creating conflict zones for each episode. This may take a moment.")
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
  basedata <- basedata %>%
    left_join(zones_nogeom, by = c("conflict_epi_id" = "epi_id")) %>%
    mutate(missingzone = if_else(is.na(haszone), 1, 0)) %>% select(-c(haszone,source_z))

  # Buffer only if specified
  if (buffer_percent > 0) {
    message("Buffering conflict zones")
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

  # Country border clipping
  if (!clipcountry) {
    message("Clipping skipped. Saving combined polygons...")
    return(list(basedata = basedata, episode_zones = combined_polygons))
  } else {
    message("Clipping conflict zones at relevant country borders...")
    episodesannual <- basedata %>% filter(active==1) %>% distinct(conflict_epi_id,year, .keep_all = T) %>%  select(conflict_epi_id,conflict_id,gwno_a,year) %>% rename(confcountry = gwno_a)

    # Find out where most events take place (the "event" country)
    ged <- downloaducdp("ged")
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
    process_conflict_zone <- function(row, relcountrybyepi, cshapes) {
      epi_id <- row$epi_id
      source_z_val <- row$source_z   # Capture the source_z value from the row
      conflict_geometry <- row$geometry

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

      # Step 1e: Return the result as an sf object (tibble)
      if (!is.null(clipped_zone) && length(clipped_zone) > 0) {
        return(tibble(epi_id = epi_id, source_z = source_z_val, geometry = st_union(clipped_zone)))
      } else {
        message(paste("No valid intersection for epi_id:", epi_id))
        return(NULL)
      }
    }

    # Step 1: Apply the clipping function rowwise
    clipped_zones_list <- combined_polygons %>%
      rowwise() %>%
      mutate(
        clipped = list(process_conflict_zone(cur_data(), relcountrybyepi, cshapes))
      ) %>%
      ungroup()

    # Step 2: Combine all clipped geometries into a single sf object
    clipped_zones <- do.call(rbind, clipped_zones_list$clipped)

    # Step 3: Ensure it's an sf object
    clipped_zones <- st_as_sf(clipped_zones)

    # Save clipped zones to the same file name
    message("Saving clipped conflict zones...")
    return(list(basedata = basedata, episode_zones = clipped_zones))
  }
}

