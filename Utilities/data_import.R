# ----------------------------------
# Tools
# ----------------------------------
# 
# 
#' Read file from Google Drive using the google id hash
#' adapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R
#'
#' @param file_id_gdrive Google Drive csv file id A character
#' @param gdrive_url Google Drive URL A character
#'
#' @return data frame with the csv content A data frame
#' @export
#'
#' @examples 
#' my_data <- read_csv_gdrive("0B7AABlvKD6WjSTY3YUVKZ1AwLWs")
#' 
read_csv_gdrive <- function(file_id_gdrive, skipper=0, gdrive_url="https://drive.google.com/uc?export=download&id="){
  # Create the full URL for the files
  download_link <- paste0(gdrive_url,file_id_gdrive)
  # Import the csv as Data frame
  data_df <- read.csv(file = download_link, header = T, skip = skipper)
  return(data_df)
}