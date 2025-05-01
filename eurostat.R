library(eurostat)

x <- search_eurostat("young")
knitr::kable(head(x))

# "Employment rates of young people not in education and training by sex, educational attainment level, years since completion of highest level of education and NUTS 2 region"  
dat <- get_eurostat("edat_lfse_33", time_format = "num", stringsAsFactors = TRUE, filters = list(lastTimePeriod = 1), type = "label")
datf <- dat %>% filter(sex=="Total" & isced11=="All ISCED 2011 levels" & duration=="Total" & age =="From 15 to 34 years" & time==2023)

print(label_eurostat_vars(id = "tran_hv_ms_psmod", names(dat)))

library(giscoR)
nuts2 <- gisco_get_nuts(nuts_level = 2)


theme_set(theme_bw(20))
library(ggplot2)

borders <- gisco_get_countries(epsg = "3035", year = "2020", resolution = "3")

eu_bord <- borders %>%
  filter(CNTR_ID %in% nuts2$CNTR_CODE)

nuts2_sf <- nuts2 %>%
  left_join(dat, by = "geo") %>%
  mutate(
    values_th = values / 1000,
    categ = cut(values_th, c(0, 15, 30, 60, 90, 120, Inf))
  )

# Finally the plot
ggplot(nuts2_sf) +
  # Background
  geom_sf(data = borders, fill = "#e1e1e1", color = NA) +
  geom_sf(aes(fill = categ), color = "grey20", linewidth = .1) +
  geom_sf(data = eu_bord, fill = NA, color = "black", linewidth = .15) +
  # Center in Europe: EPSG 3035
  coord_sf(xlim = c(2377294, 6500000), ylim = c(1413597, 5228510)) +
  # Legends and color
  scale_fill_manual(
    values = hcl.colors(length(labs), "Geyser", rev = TRUE),
    # Label NA
    labels = function(x) {
      ifelse(is.na(x), "No Data", x)
    },
    na.value = "#e1e1e1"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_void() +
  theme(
    text = element_text(colour = "grey0"),
    panel.background = element_rect(fill = "#97dbf2"),
    panel.border = element_rect(fill = NA, color = "grey10"),
    plot.title = element_text(hjust = 0.5, vjust = -1, size = 12),
    plot.subtitle = element_text(
      hjust = 0.5, vjust = -2, face = "bold",
      margin = margin(b = 10, t = 5), size = 12
    ),
    plot.caption = element_text(
      size = 8, hjust = 0.5, margin =
        margin(b = 2, t = 13)
    ),
    legend.text = element_text(size = 7, ),
    legend.title = element_text(size = 7),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text.position = "bottom",
    legend.title.position = "top",
    legend.key.height = rel(0.5),
    legend.key.width = unit(.1, "npc")
  ) +
  # Annotate and labels
  labs(
    title = "Disposable income of private households (2021)",
    subtitle = "NUTS-2 level",
    fill = "euros (thousands)",
    caption = paste0(
      "Source: Eurostat\n ", gisco_attributions()
    )
  )


# Select specific regions
# select_nuts <- gisco_get_nuts(nuts_id = c("ES2", "FRJ", "FRL", "ITC"))
#ggplot(select_nuts) +
#  geom_sf(aes(fill = CNTR_CODE)) +
#  scale_fill_viridis_d()
