box::use(
  readr[read_csv],
  here[here],
  lubridate[ymd_hm, interval, int_length],
  dplyr[...],
  ggplot2[...],
  tidyr[...],
  forcats[...]
)

fasting_df <- readr::read_csv(file = here::here("raw_data", "Imsak and Futoor Time in Ramadhan 2024 _ 15Mar2024 - Sheet1.csv"))


fasting_df |>
  dplyr::select(-c(data_source, month, country, city)) |>
  dplyr::mutate(
    # parse data/time
    imsak_time = lubridate::ymd_hm(fasting_df$imsak_time),
    futoor_time = lubridate::ymd_hm(fasting_df$futoor_time)
  ) |>
  dplyr::mutate(
    # create interval
    fasting_interval = lubridate::interval(start = imsak_time, end = futoor_time)
  ) |>
  dplyr::mutate(
    # find interval length in hours
    fasting_in_hours = round(lubridate::int_length(fasting_interval) / 3600, 2)
  ) -> fasting_df


fasting_df |>
  tidyr::pivot_longer(
    cols = c(imsak_time, futoor_time),
    names_to = "time",
    values_to = "date"
  ) |>
  dplyr::select(-c("fasting_interval")) -> graph_df

graph_df |>
  dplyr::mutate(
    time_decimal = round(lubridate::hour(date) + (lubridate::minute(date) / 60), 2),
    time_label = paste0(lubridate::hour(date), ":", lubridate::minute(date))
  ) |>
  dplyr::mutate(month_ar = forcats::as_factor(month_ar)) -> graph_df

ggplot2::ggplot(graph_df) +
  ggplot2::geom_point(mapping = aes(x = time_decimal, y = month_ar)) +
  theme_bw()
