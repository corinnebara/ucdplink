#' Helper function (internal) to download UCDP datasets
#'
#' @param dataset Which of four datasets to download: can be acd, ged, osv, nsv
#'
#' @return The dataset as an object (dataframe) in the global environment
#' @keywords internal
#'
#' @importFrom utils download.file unzip
#'
#' @examples
#' \dontrun{
#'   downloaducdp("acd")
#' }
downloaducdp <- function(dataset) {
  # Ensure the dataset name is lowercase and valid.
  valid_datasets <- c("acd", "ged", "osv", "nsv")
  dataset <- tolower(dataset)
  if (!dataset %in% valid_datasets) {
    stop("Invalid dataset. Choose one of: ", paste(valid_datasets, collapse = ", "))
  }

  # Define the URLs for each dataset
  urls <- list(
    acd = "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-241-rds.zip",
    ged = "https://ucdp.uu.se/downloads/ged/ged241-rds.zip",
    osv = "https://ucdp.uu.se/downloads/nsos/ucdp-onesided-241-rds.zip",
    nsv = "https://ucdp.uu.se/downloads/nsos/ucdp-nonstate-241-rds.zip"
  )

  # Select the URL based on the dataset argument.
  url <- urls[[dataset]]

  # Create a temporary file to save the ZIP file.
  temp_zip <- tempfile(fileext = ".zip")

  # Download the ZIP file quietly (binary mode is important on Windows).
  download.file(url, destfile = temp_zip, mode = "wb", quiet = TRUE)

  # Create a temporary directory to unzip the contents.
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Unzip the downloaded file into the temporary directory quietly.
  suppressMessages({
    unzip(temp_zip, exdir = temp_dir)
  })

  # Look for the RDS file in the temporary directory.
  rds_files <- list.files(temp_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0) {
    stop("No RDS file found in the unzipped archive.")
  }

  # Read the first RDS file.
  data_obj <- readRDS(rds_files[1])

  # Ensure that the resulting object is a data frame.
  if (!is.data.frame(data_obj)) {
    data_obj <- as.data.frame(data_obj)
  }

  # Assign the data frame to the global environment with the name of the dataset.
  return(data_obj)

  # Clean up the temporary files for later.
  unlink(c(temp_zip, temp_dir), recursive = TRUE)
  invisible(data_obj)
}
