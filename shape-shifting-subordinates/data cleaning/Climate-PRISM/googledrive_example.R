library(googledrive)

jrn_folder <- "https://drive.google.com/drive/u/0/folders/12j4v55xekEdyLocYkfpx8o5kykQfOC2D"

file_we_want <- googledrive::drive_ls(path = as_id(jrn_folder), type = "csv", pattern = "JornadaStudy")

file_id <- file_we_want$id

# Download file
googledrive::drive_download(file = as_id(file_id), 
                            overwrite = T)

