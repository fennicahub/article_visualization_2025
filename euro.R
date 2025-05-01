library(giscoR)
library(sf)
library(ggplot2) # Use ggplot for plotting
library(dplyr)
library(eurostat)

theme_set(theme_bw(30))

# EU members
nuts2 <- gisco_get_nuts(year = "2024", epsg = "3035", resolution = "10", nuts_level = "2")

# Borders from countries
borders <- gisco_get_countries(epsg = "3035", year = "2024", resolution = "3")

eu_bord <- borders %>%
  filter(CNTR_ID %in% nuts2$CNTR_CODE)

# Eurostat data - Disposable income
# pps <- get_eurostat("tgs00026") %>% filter(TIME_PERIOD == "2022-01-01")

# "Employment rates of young people not in education and training by sex, educational attainment level, years since completion of highest level of education and NUTS 2 region"  
dat <- get_eurostat("edat_lfse_33", time_format = "num", stringsAsFactors = TRUE, filters = list(lastTimePeriod = 1))
datf <- dat %>% filter(sex=="T" & isced11=="TOTAL" & duration=="TOTAL" & age =="Y15-34" & time==2023)
pps <- datf %>% filter(!is.na(values))

quantiles <- c(-Inf, quantile(pps$values, seq(0.25, 0.75, 0.25)), Inf)

nuts2_sf <- nuts2 %>%
  left_join(pps, by = "geo") %>%
  mutate(
    values_th = values,
    # categ = cut(values_th, c(0, 15, 30, 60, 90, 120, Inf))
    categ = cut(values_th, quantiles)    
  )


# Adjust the labels
# labs <- levels(nuts2_sf$categ)
labs <- paste0("Q", 1:4)
#labs[1] <- "< 15"
#labs[6] <- "> 120"
levels(nuts2_sf$categ) <- labs

# Finally the plot
p <- ggplot(nuts2_sf) +
  # Background
  geom_sf(data = borders, fill = "#e1e1e1", color = NA) +
  geom_sf(aes(fill = categ), color = "grey20", linewidth = .1) +
  geom_sf(data = eu_bord, fill = NA, color = "black", linewidth = .15) +
  # Center in Europe: EPSG 3035
  coord_sf(xlim = c(2377294, 6500000), ylim = c(1413597, 5228510)) +
  # Legends and color
  scale_fill_manual(
    values = hcl.colors(length(labs), "Geyser", rev = FALSE),
    #values = rev(c("darkblue","blue","red","darkred")),    
    # Label NA
    labels = function(x) {
      ifelse(is.na(x), "No Data", x)
    },
    na.value = "#e1e1e1"
  ) +
  guides(fill = guide_legend(nrow = 1, reverse=FALSE)) +
  theme_void() +
  theme(
    text = element_text(colour = "grey0"),
    panel.background = element_rect(fill = "#97dbf2"),
    panel.border = element_rect(fill = NA, color = "grey10"),
    plot.title = element_text(hjust = 0.5, vjust = -1, size = 15),
    plot.subtitle = element_text(
      hjust = 0.5, vjust = -2, face = "bold",
      margin = margin(b = 10, t = 5), size = 15
    ),
    plot.caption = element_text(
      size = 10, hjust = 0.5, margin =
        margin(b = 2, t = 13)
    ),
    legend.text = element_text(size = 10, ),
    legend.title = element_text(size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text.position = "bottom",
    legend.title.position = "top",
    legend.key.height = rel(0.5),
    legend.key.width = unit(.1, "npc")
  ) +
  # Annotate and labels
  labs(
    title = "Youth employment rates (2023)",
    subtitle = "NUTS-2 level",
    fill = "Quantile",
    caption = paste0(
      "Source: Eurostat\n ", gisco_attributions()
    )
  )



library(Cairo)
CairoJPEG("fig4.3.jpeg", width=500, height=500)
print(p)
dev.off()