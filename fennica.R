
library(tidyverse)
library(ggplot2)

# Fetch Fennica data from Rahti:
# https://fennica-fennica.2.rahtiapp.fi/harmonized_fennica.html
# f <- read.csv2("https://a3s.fi/swift/v1/AUTH_3c0ccb602fa24298a6fe3ae224ca022f/fennica-container/output.tables/harmonized_fennica19.csv", sep ="\t")

#fig. 4.1 Data preparation and quality control: Fennica?
#- quality control (reveal problems)
#- exploration, hypothesis generation and verification

# Authors with most entries
top <- names(head(rev(sort(table(f$author_name))), 5))
f$author_birth <- as.numeric(f$author_birth)
f$author_death <- as.numeric(f$author_death)

fs <- subset(f, author_name %in% top) %>%
        select(author_name, author_birth, author_death) %>%
	unique() %>%
	arrange(author_birth) %>%
	mutate(author_name=factor(author_name, levels=unique(author_name)))

theme_set(theme_bw(30))
p <- ggplot(fs, aes(x=author_birth, y=author_name)) +
       geom_segment(aes(xend=author_death),
         arrow=arrow()) + 
	 labs(x="Year", y="") +
	 theme(legend.position="none")

library(Cairo)
CairoJPEG("fig4.1.jpeg", width=1000, height=500)
print(p)
dev.off()