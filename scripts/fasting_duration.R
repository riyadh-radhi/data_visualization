box::use(
  readr[read_csv],
  here[here],
  lubridate[ymd_hm, interval, int_length],
  dplyr[...],
  ggplot2[...],
  tidyr[...],
  forcats[...],
  rtlr[str_rtl],
  stringr[...],
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

fasting_df <- fasting_df |>
  dplyr::mutate(
    imsak_decimal = round(lubridate::hour(imsak_time) + (lubridate::minute(imsak_time) / 60), 2),
    futoor_decimal = round(lubridate::hour(futoor_time) + (lubridate::minute(futoor_time) / 60), 2),
    month_ar = forcats::as_factor(month_ar),
    imsak_label = paste0(lubridate::hour(imsak_time), ":", lubridate::minute(imsak_time)),
    futoor_label = paste0(lubridate::hour(futoor_time), ":", lubridate::minute(futoor_time))
  )

fasting_df <- fasting_df |>
  mutate(
    middle_point = (imsak_decimal + futoor_decimal) / 2,
    fasting_in_hours = paste0(fasting_in_hours, " ساعة من الصيام ")
  )

# graph_df |>
#   dplyr::mutate(
#     time_decimal = round(lubridate::hour(date) + (lubridate::minute(date) / 60), 2),
#     time_label = paste0(lubridate::hour(date), ":", lubridate::minute(date))
#   ) |>
#   dplyr::mutate(month_ar = forcats::as_factor(month_ar)) -> graph_df

# Define a gradient color scale
gradient_scale <- scale_color_gradient(low = "#A8E6CE", high = "#FF6B6B")

# Your original ggplot code with gradient color scale added
ggplot2::ggplot(fasting_df) +
  geom_segment(
    mapping = aes(x = imsak_decimal, y = month_ar, xend = futoor_decimal, color = futoor_decimal - imsak_decimal),
    linewidth = 10, lineend = "round"
  ) +
  gradient_scale +
  geom_text(mapping = aes(x = imsak_decimal - 4, y = month_ar, label = rtlr::str_rtl(paste0(imsak_label, " الفجر"))), size = 2) +
  geom_text(mapping = aes(x = futoor_decimal + 4, y = month_ar, label = rtlr::str_rtl(paste0(futoor_label, " المغرب"))), size = 2) +
  geom_text(mapping = aes(x = middle_point, y = month_ar, label = rtlr::str_rtl(fasting_in_hours)), size = 2) +
  labs(
    x = rtlr::str_rtl("اوقات الصيام"), y = rtlr::str_rtl("الشهر الميلادي"),
    title = stringr::str_wrap(rtlr::str_rtl("عدد ساعات الصيام يختلف هواي اذا رمضان صادف بشهر السابع مقارنة بشهر الواحد بالعراق :)"), width = 60),
    caption = stringr::str_wrap(rtlr::str_rtl("الرسم البياني صمم بااستعمال لغة البرجمة R"), width = 60),
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 7, face = "bold"),
    plot.title = element_text(
      size = 8, hjust = 1
    ),
    plot.caption = element_text(
      hjust = 1,
      size = 4
    )
  ) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(expand = expand_scale(
    add = c(2, 3)
  ))

ggsave(here::here("output", "fasting_duration.png"), width = 1080 / 300, height = 1350 / 300, units = "in", dpi = 300)
