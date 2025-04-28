

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

str(combined_df)

trimmed_df <- combined_df %>%
  filter(
    # compute “deploy date at 7 PM” for each record
    date_time_pdt >= (ymd(date_deployed, tz = "America/Los_Angeles") + hours(19)),
    # keep only timestamps before 2025-04-14 00:00 PDT
    date_time_pdt <  ymd("2025-04-14", tz = "America/Los_Angeles")
  )

# Inspect
glimpse(trimmed_df)


################################################################################
# Plot daily mean


# Compute daily mean per site
daily_site_means <- trimmed_df %>%
  mutate(date = as_date(date_time_pdt)) %>%
  group_by(site = kr_site_id, date) %>%
  summarise(
    mean_temp = mean(temperature_c, na.rm = TRUE),
    .groups = "drop"
  )

#Compute across-site summary per day
daily_summary <- daily_site_means %>%
  group_by(date) %>%
  summarise(
    mean_across_sites = mean(mean_temp),
    se_across_sites   = sd(mean_temp) / sqrt(n()),
    lower             = mean_across_sites - se_across_sites,
    upper             = mean_across_sites + se_across_sites,
    .groups = "drop"
  )


# Make sure you have date on your raw data:
raw_plot_df <- trimmed_df %>%
  mutate(date = as_date(date_time_pdt))

p <- ggplot() +
  # 1) All raw readings as points
  geom_point(
    data = raw_plot_df,
    aes(x = date, y = temperature_c),
    alpha = 0.1,    # lighten for overplotting
    size  = 0.6
  ) +
  # 2) Ribbon for mean ± SE across sites
  geom_ribbon(
    data = daily_summary,
    aes(x = date, ymin = lower, ymax = upper, group = 1),
    fill  = "grey80",
    alpha = 0.5
  ) +
  # 3) Line for the across‐site daily mean
  geom_line(
    data = daily_summary,
    aes(x = date, y = mean_across_sites),
    color = "steelblue",
    size  = 1.5
  ) +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%b %d"
  ) +
  labs(
    title = "Daily mean temperature across recovery sites",
    x     = "Date",
    y     = expression("Temperature ("*~degree*C*")")
  ) +
  theme_bw()



out_file <- file.path(Sys.getenv("HOME"), "Downloads", "daily_temps_plot.png")

ggsave(
  filename = out_file,
  plot     = last_plot(),  # or: plot = my_ggplot_object
  width    = 10,           # inches
  height   = 5,            # inches
  dpi      = 300,          # resolution
  device   = "png"
)

message("Saved plot to: ", out_file)

