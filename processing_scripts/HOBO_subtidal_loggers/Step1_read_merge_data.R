

rm(list=ls())

################################################################################
# load packages and set directories

require(librarian)
librarian::shelf(here, tidyverse, readxl, purrr, stringr, lubridate, 
                 googledrive, janitor, googlesheets4)

#authenticate Google drive, where the data are stored
drive_auth()

#read metadata
ss <- "1KqHkhc7o7iFFU-BAYf84w35PsXi2-oOlLAoU5mkUgsc"
metadata <- googlesheets4::read_sheet(ss, sheet = 1) %>%
            clean_names()

#List all .xlsx files in the target folder
folder_id <- "1fT_pGLAujSPYA0iz0moYyU2u2uKlwh9F"
files <- drive_ls(as_id(folder_id)) %>%
  filter(str_detect(name, "\\.xlsx$"))

#Download each file to a temp location and read it in
data_list <- files %>%
  mutate(
    temp_path = map_chr(id, ~ {
      tmp <- tempfile(fileext = ".xlsx")
      drive_download(as_id(.x), path = tmp, overwrite = TRUE)
      tmp
    }),
    df = map(temp_path, read_excel)
  ) %>%
  #Extract serial number and timestamp from the filename
  mutate(
    serial = str_extract(name, "^[^ ]+"),
    timestamp = name %>%
      str_extract("\\d{4}-\\d{2}-\\d{2} \\d{2}_\\d{2}_\\d{2}") %>%
      str_replace_all("_", ":") %>%
      ymd_hms(tz = "America/Los_Angeles")
  ) %>%
  #Attach those metadata columns to each sheet
  mutate(
    df = pmap(
      list(df, serial, timestamp),
      ~ mutate(..1,
               serial_number = ..2,
               downloaded_at  = ..3)
    )
  ) %>%
  clean_names()

#Combine all into one data.frame
combined_df <- map_dfr(data_list$df, identity) %>% clean_names() %>%
                mutate(serial_number = as.numeric(serial_number)) %>%
                #combine metadata
                left_join(., metadata, by = c("serial_number" = "serial"))

#Quick look at the result
glimpse(combined_df)

################################################################################
# Tidy data









