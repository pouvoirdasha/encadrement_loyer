# ============================================================
# Download data from Google Drive
# Files will be downloaded in the same folder as this script.
# ============================================================

required_packages <- c("googledrive", "rstudioapi")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(googledrive)

# ------------------------------------------------------------
# 1. Use Google Drive without authentication
# ------------------------------------------------------------

# Required if files are shared as:
# "Anyone with the link can view"
drive_deauth()

# ------------------------------------------------------------
# 2. Find the folder where this script is located
# ------------------------------------------------------------

get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(script_path)) {
      return(dirname(script_path))
    }
  }
  
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg))
    return(dirname(script_path))
  }
  
  return(getwd())
}

output_dir <- get_script_dir()

message("Files will be downloaded to: ", output_dir)

# ------------------------------------------------------------
# 3. List of files to download
# ------------------------------------------------------------

# Replace the IDs below with your Google Drive file IDs.
# The files must be shared as:
# "Anyone with the link can view"

files_to_download <- data.frame(
  file_name = c(
    "commune_2022.csv",
    "base_2006_2022_avec_aire.csv",
    "clusters_paris_sans_lille.csv",
    "codes_communes_cluster_lille_sans_paris_lille.csv"
  ),
  drive_id = c(
    "1xM-aBi_w5-BX1-pIU2KCC0pb-ZakJeQ1",
    "1tltMlxAoPW2Vl-GWmdnXYj_Y65D8DSi_",
    "1YOF7Bqi3p2_RHEM1G6kkupAowAZTLg1H",
    "1IRywyp1nmS3ePjpqJH_orzGQgMRPM1FF"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 4. Download function
# ------------------------------------------------------------

download_one_file <- function(file_name, drive_id, output_dir, overwrite = FALSE) {
  
  output_path <- file.path(output_dir, file_name)
  
  if (file.exists(output_path) && overwrite == FALSE) {
    message("Already exists, skipping: ", file_name)
    return(invisible(output_path))
  }
  
  message("Downloading: ", file_name)
  
  tryCatch(
    {
      googledrive::drive_download(
        file = googledrive::as_id(drive_id),
        path = output_path,
        overwrite = overwrite
      )
      
      message("Saved: ", output_path)
      return(invisible(output_path))
    },
    error = function(e) {
      message("Could not download: ", file_name)
      message("Reason: ", e$message)
      message("Check that the file is shared as 'Anyone with the link can view'.")
      return(invisible(NULL))
    }
  )
}

# ------------------------------------------------------------
# 5. Download all files
# ------------------------------------------------------------

for (i in seq_len(nrow(files_to_download))) {
  download_one_file(
    file_name = files_to_download$file_name[i],
    drive_id = files_to_download$drive_id[i],
    output_dir = output_dir,
    overwrite = FALSE
  )
}

message("Download finished.")