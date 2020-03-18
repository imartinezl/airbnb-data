library(dplyr)

df <- read.csv(‘./data/listings.csv', stringsAsFactors = F)

df$first_review
df$last_review
df$longitude
df$latitude


date_ <- df %>% 
  dplyr::mutate(first_review = as.Date(first_review)) %>% 
  dplyr::sample_n(1) %>% 
  dplyr::pull(first_review)

bbox <- c(left = -3.5, bottom = 42.40, right = -1.55, top = 43.80) # euskadi
euskadi_map <- ggmap::get_stamenmap(bbox, zoom = 9, maptype = "terrain-background") %>% 
  ggmap::ggmap()

bbox <- c(left = -2.04, bottom = 43.28, right = -1.93, top = 43.35) # donostia
donostia_map <- ggmap::get_stamenmap(bbox, zoom = 14, maptype = "toner-lines") %>% 
  ggmap::ggmap()
  
dates <- df$first_review %>% as.Date() %>% unique() %>% sort()
dates_seq <- seq(as.Date("2011-06-01"), as.Date("2020-02-01"), "weeks")
for(i in 1:length(dates_seq)){
  d <- df %>% 
    dplyr::mutate(first_review = as.Date(first_review),
                  last_review = as.Date(last_review),
                  host_since = as.Date(host_since)) %>% 
    dplyr::select(id, first_review, last_review, host_since, longitude, latitude) %>% 
    na.omit() %>% 
    dplyr::filter(first_review <= dates_seq[i])
  
  euskadi_map +
    ggplot2::geom_point(data=d, ggplot2::aes(x=longitude, y=latitude, group=first_review),
                        size=0.5, alpha=0.3, shape=16, color="#ff0055")+
    ggplot2::geom_text(x=-3.45, y=43.75, label="Basque Country Airbnb Rentals",
                       hjust=0, vjust=1, size=4, family="Bebas Neue", check_overlap = T)+
    ggplot2::geom_text(x=-1.6, y=43.75, label=dates_seq[i],
                       hjust=1, vjust=1, size=4, family="Bebas Neue", check_overlap = T)+
    cowplot::theme_nothing()+
    ggplot2::ggsave(filename = paste0("figures/euskadi_", i, ".png"), device = "png", width=4, height=4)

  donostia_map +
    ggplot2::geom_point(data=d, ggplot2::aes(x=longitude, y=latitude, group=first_review), 
                        size=0.5, alpha=0.3, shape=16, color="#ff0055", na.rm=T)+
    ggplot2::geom_text(x=-2.035, y=43.348, label="San Sebastian Airbnb Rentals",
                       hjust=0, vjust=1, size=4, family="Bebas Neue", check_overlap = T)+
    ggplot2::geom_text(x=-1.935, y=43.348, label=dates_seq[i],
                       hjust=1, vjust=1, size=4, family="Bebas Neue", check_overlap = T)+
    cowplot::theme_nothing()+
    ggplot2::ggsave(filename = paste0("figures/donostia_alt_", i, ".png"), device = "png", width=4, height=4)
}


