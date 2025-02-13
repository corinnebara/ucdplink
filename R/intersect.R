#' Intersects the GED event dataset with the conflict zones for later spatial linking
#'
#' @param gedprepped Data passed on from gedprep
#' @param episode_zones Data passed on from makezones
#'
#' @return A reduced GED event dataset where each event has one or several conflict zones assigned (one event = several rows if it falls in more than one zone)
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import sf
#' @import future
#' @import furrr
#'
#' @examples
#' \dontrun{
#'   intersect(gedprepped,episode_zones)
#' }
intersect <- function(gedprepped,episode_zones) {

  # Drop battle violence (never spatially linked) to keep computing time at bay
  ged <- gedprepped %>% filter(type_of_violence!=1)

  # Drop events that are not geo precision 1-3
  ged <- ged %>% filter(where_prec<=3)

  # Prepare intersection
  gedspace <- st_as_sf(ged, coords = c("longitude", "latitude"),  crs = 4326)
  gedspace <- st_make_valid(gedspace)

  # Make the intersection, speed up with parallel computing
  compute_intersection <- function(gedspace, episode_zones) {
    # Check if input datasets are valid sf objects
    if (!inherits(gedspace, "sf") | !inherits(episode_zones, "sf")) {
      stop("Both gedspace and zones must be valid sf objects.")
    }

    # Set up the future plan (automatic handling for Windows, macOS, Linux)
    plan(multisession)  # Automatically adjusts to the user's system

    # Split gedspace into chunks based on available cores
    n_cores <- future::availableCores()
    chunks <- split(gedspace, cut(seq_len(nrow(gedspace)), n_cores, labels = FALSE))

    # Perform parallel intersection
    intersections <- future_map(chunks, ~ st_intersection(.x, episode_zones))

    # Combine results into a single sf object
    gedinter <- do.call(rbind, intersections)

    return(gedinter)
  }

  gedinter <- compute_intersection(gedspace, episode_zones)

  # Check and save (rename variables that were attached by the zones, e.g., info comes from conflict zones). # Unique identifier: idsplit,side_a_new_id,side_b_new_id,z_conf_id
  gedinter <- gedinter %>% rename(z_conf_id = epi_id) %>% st_drop_geometry()

  # Return the intersection. It contains ALL Type 2/3 violence, also the type we link by actor
  return(gedinter)
}
