
#v <- fetch_viola_records(
#    base_query = "*",
#    base_filters = c('collection:"VIO"'), # Filters for the Viola collection
#    include_na = FALSE,                   # Include records with missing dates
#    limit_per_query = 100000,              # Maximum records per query
#    total_limit = Inf,                 # Overall record limit
#    delay_after_query = 3                # Delay between API calls
#)
#print(head(v))


# fig. 4.2 Simplicity: Viola?
# Eliminate unnecessary elements
# Multiple levels of details
# such as duplicated information represented simultaneously by multiple elements
# (i.e., colors and symbols), and entirely avoiding decorations that do not add
# information in the visual presentation. Possibly an example visualization of
# this and showing the data points -> (under/oversimplify)

v$Year <- as.numeric(v$Year)

d <- v %>% group_by(Year, Formats) %>%
           summarise(n=n()) %>%
	   filter(Formats %in% names(tail(sort(table(v$Formats)), 2))) %>%
	   filter(Year > 1960)

d$Formats <- gsub("Äänite, Äänilevy",    "Disk", d$Formats)
d$Formats <- gsub("Äänite, CD",          "CD", d$Formats)
#d$Formats <- gsub("Äänite, Musiikkitallenne", "Music recording", d$Formats)
#d$Formats <- gsub("Äänite, Äänikasetti", "Cassette", d$Formats)

theme_set(theme_bw(20))
p1 <- ggplot(d, aes(x=Year, y=n, color=Formats, shape=Formats)) +
       geom_point(size=3) +
       geom_smooth(size=1)

theme_set(theme_bw(20))
p3 <- ggplot(d, aes(x=Year, y=n, color=Formats)) +
       geom_smooth(size=1)

theme_set(theme_bw(20))
p0 <- ggplot(d, aes(x=Year, y=n, group=Formats)) +
       geom_smooth(size=1, color="black")

theme_set(theme_bw(30))
p2 <- ggplot(d, aes(x=Year, y=n, color=Formats)) +
       geom_point(size=3) +
       geom_smooth(size=1) +
       scale_color_manual(values=c("black", "darkgray")) +
       labs(x="Year", y="Records (n)", color="") +
       theme(legend.position=c(0.08, 0.82)) 


#library(patchwork)
#p <- p0 + p3 + p2 + p1 
#print(p)

library(Cairo)
CairoJPEG("fig4.2.jpeg", width=850, height=500)
print(p2)
dev.off()
