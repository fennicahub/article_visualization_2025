# Timeline of Fennica title counts, 1809-1917
#
# Reads the harmonized 1809-1917 Fennica subset published to CSC Allas by this
# project (written by inst/examples/harmonized_fennica.R). Everything needed
# here is already computed in it: publication_year, publication_year_till,
# publication_status, data_element and biblio_level.
#
# Reader note: the file has unescaped quote characters in title fields, so a
# default-quoting reader such as read.csv2() stops mid-record and silently
# returns 67873 rows instead of 72626. quote = "" is required. read.csv2 would
# also apply dec = ",", leaving the year columns as character.
#
# Counting is done with finna::timeline() over books only: data_element
# "Kirjat" in the pipeline's own material classification, which leaves out
# "Jatkuvat julkaisut" (periodicals and other continuing resources) along with
# maps, music and visual material. Every record is counted once, at its
# publication_year.
#
# Excluded: the 008 status codes "q" (questionable date) and "m" (multiple
# dates). Those are brackets on undated items rather than publication years,
# and Date 1 parks them all on the first year of the bracket -- over a thousand
# of them land on 1810 alone. publication_year_till cannot be used to catch
# them, because the pipeline truncates the field to Date 1 before
# polish_years_008() sees it, leaving till empty for exactly these records.

start_year <- 1880
end_year   <- 1900

#start_year <- 1809
#end_year   <- 1917

# ---- 2. Restrict to the period, drop bracketed dates -------------------------
df19 <- df %>%
  mutate(
    publication_year      = suppressWarnings(as.integer(publication_year)),
    publication_year_till = suppressWarnings(as.integer(publication_year_till)),
    bracketed   = publication_status %in% c("Questionable date", "Multiple dates"),
    single_year = is.na(publication_year_till) |
                  publication_year_till == publication_year
  ) %>%
  filter(!is.na(publication_year), !bracketed,
         publication_year >= start_year, publication_year <= end_year)

# ---- 3. Count book titles per year, and plot ---------------------------------
tl <- timeline(df19 %>% filter(data_element == "Kirjat"),
               field       = "titlecount",
               time.window = 1,
               time.field  = "publication_year")

titles_per_year <- tibble(year = start_year:end_year) %>%
  left_join(tl %>% ungroup() %>%
              transmute(year = publication_time, titles = absolute),
            by = "year") %>%
  mutate(titles = tidyr::replace_na(titles, 0L)) %>%
  arrange(year)

write.csv(titles_per_year, "fennica_titles_per_year_1809_1917.csv", row.names = FALSE)

highlight <- c(1892, 1896)   # period to shade

p <- ggplot(titles_per_year, aes(x = year, y = titles)) +
  annotate("rect", xmin = highlight[1], xmax = highlight[2],
           ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.25) +
  geom_line(colour = "steelblue4", linewidth = 0.5) +
  geom_point(size = 1, colour = "steelblue4") +
  # pad rather than clip, so the first and last bars are drawn whole
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6),
                     expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = paste0("Fennica: book titles per year, ", start_year, "-", end_year),
    x = "Publication year", y = "Title count (N)"
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

#ggsave("fennica_timeline_1809_1917.png", p, width = 9, height = 5, dpi = 300)
print(p)

library(Cairo)
v <- 1000
CairoJPEG("fig4.6.jpeg", width=2*v, height=v, dpi = 300, quality = 100, bg = "white")
print(p)
dev.off()


