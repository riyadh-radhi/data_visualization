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
    gghighlight[...],
    ggtext[...],
    stringi[...]
)

rain_intensity_data <- readr::read_csv(file = here::here("raw_data", "Iraq Rains Level _ 17-22March2024 - Sheet1.csv"))



rain_intensity_data |>
    dplyr::mutate(
        intensity_category = dplyr::case_when(
            rain_intensity_mm_per_hour < 2.5 ~ "خفيف: اقل من 2.5 ملم في الساعة",
            rain_intensity_mm_per_hour >= 2.5 & rain_intensity_mm_per_hour <= 10 ~ "متوسط: من 2.5 الى 10 ملم في الساعة",
            rain_intensity_mm_per_hour > 10 ~ "غزير: اكثر من 10 ملم في الساعة"
        )
    ) -> plot_data


plot_data |>
    dplyr::group_by(province) |>
    dplyr::summarise(rain_average = mean(rain_intensity_mm_per_hour))


plot_data |>
    dplyr::group_by(province, date) |>
    dplyr::summarise(rain_average = mean(rain_intensity_mm_per_hour)) -> plot_data


reverse_string <- function(x) {
    paste(rev(strsplit(x, " +")[[1]]), collapse = " ")
}

colors_vector <- c("#6BA4B8", "#5CA05C", "#7043AA", rep("gray", 12))

plot_data |>
    dplyr::mutate(
        fill = dplyr::case_when(
            province == "بغداد" ~ "#305766",
            province == "البصرة" ~ "#376d37",
            province == "نينوى" ~ "#3c1e63",
            TRUE ~ "#afafaf3b"
        ),
        color = dplyr::case_when(
            province == "بغداد" ~ "#6BA4B8",
            province == "البصرة" ~ "#5CA05C",
            province == "نينوى" ~ "#7043AA",
            TRUE ~ "#afafaf3b"
        )
    ) -> plot_data


title_input <-
    paste0(
        reverse_string(" معدل شدة الامطار الاسبوع الماضي "),
        reverse_string(" اعلى ثلاث محافظات في "),
        "<br></br>",
        "<span style='color:#305766'>", reverse_string("بغداد"), "</span>",
        "و ,",
        "<span style='color:#3c1e63'>", reverse_string("نينوى"), "</span>",
        " ,",
        "<span style='color:#376d37'>", reverse_string("البصرة"), "</span>"
    )
ggplot2::ggplot(plot_data, aes(x = date, y = rain_average)) +
    geom_line(mapping = aes(color = color, group = province)) +
    geom_point(
        mapping = aes(color = color, fill = fill), shape = 21
    ) +
    labs(
        x = rtlr::str_rtl(""),
        y = rtlr::str_rtl("معدل شدة المطر (ملم/الساعة)"),
        title = stringr::str_wrap(title_input, width = 2),
        caption = paste0("التحليل لايشمل محافظات اقليم كردستان", "\n", "Data Source: Iraqi Meteorological Organization and Seismology| Data Analyst: Riyadh Radhi")
    ) +
    theme_bw() +
    theme(
        panel.border = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # axis.line = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 7),
        plot.title = ggtext::element_markdown(
            size = 8, hjust = 0.5, lineheight = 1.2
        ),
        plot.caption = element_text(
            hjust = 0,
            size = 4,
            lineheight = 1.2
        )
    ) +
    scale_y_continuous(position = "right", breaks = scales::pretty_breaks(n = 10)) +
    scale_x_discrete(expand = expand_scale(
        add = c(0.2, 0.1)
    )) +
    scale_color_identity() +
    scale_fill_identity()
ggsave(here::here("output", "rain_intensity.png"), width = 1080 / 300, height = 1350 / 300, units = "in", dpi = 300)
