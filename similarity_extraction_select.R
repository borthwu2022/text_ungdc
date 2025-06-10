
library(tidyverse)
file_path <- "C:/Users/Bort Wu/Desktop/summer_2025/projects/foreign_policy/outputs/"
  rwmd_list <- get(load(paste0(file_path, "word_vectors/rwmd_list.RData")))


simil_plot <- function(x, y){
# Pinpoint in which years speeches are available for China, Russia/USSR, USA
# and UK. There were speeches available for CHN (ROC) or RUS (USSR) in 1970. 
# Before 1972, it was the ROC representing China at UNGA. 
  
years_chn <- ungd_files$Year[ungd_files$Country == "CHN"]

years_usa <- ungd_files$Year[ungd_files$Country == "USA"]

years_gbr <- ungd_files$Year[ungd_files$Country == "GBR"]

years_rus <- ungd_files$Year[ungd_files$Country == "RUS"]


## Visualization of pairwise similarity in foreign policy preferences
# China-India, China-Japan, China-Vietnam, US-China, Africa-China, Russia-China, 
# Russia-US, EU-China, EU-US, EU-Russia, etc.

  
# list of countries of interest
list_country <- c("CHN","USA","RUS","GBR")

# Extract similarities with China and Russia
# years_chn == years_rus (ALL TRUE)

years_rus_chn <- years_chn

sim_chn_rus <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(sim_chn_rus) <- c("CHN","RUS","country","year")

for (i in years_rus_chn){
  sim_i <- x[[i-1945]] |> as.matrix() |> 
    as.data.frame() 
  countries <- rownames(sim_i) 
  year <- i 
  sim_i <- sim_i |> 
    mutate(country = countries, year = year) |> 
    select(CHN, RUS, country, year) 
  sim_chn_rus <- rbind(sim_chn_rus, sim_i)
}

sim_chn_rus <- sim_chn_rus |> 
  mutate(country_name = countrycode(country, 
                      origin = "iso3c",destination = "country.name"))

sim_chn_rus$country_name[sim_chn_rus$country=="CSK"] <- "Czechoslovakia"
sim_chn_rus$country_name[sim_chn_rus$country == "YUG"] <- "Yugoslavia"
sim_chn_rus$country_name[sim_chn_rus$country == "DDR"] <- "East Germany"
sim_chn_rus$country_name[sim_chn_rus$country == "YMD"] <- "Democratic Yemen"
sim_chn_rus$country_name[sim_chn_rus$country == "EU"] <- "European Union"


# Merge all similar scores with US & UK into one dataset

# empty data frame for similarity scores with US and UK from 1946 to 1991
sim_us_uk <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(sim_us_uk) <- c("USA","GBR","country","year")

for (i in 1:79) {
  sim.i <- x[[i]] |> as.matrix() |> as.data.frame()
  countries <- rownames(sim.i)
  year <- i + 1945
  sim.i <- sim.i |> 
    mutate(country = countries, year = year) |> 
    select(USA, GBR, country, year) 
  sim_us_uk <- rbind(sim_us_uk, sim.i)
}

sim_us_uk <- sim_us_uk |> 
  mutate(country_name = countrycode(country, 
                      origin = "iso3c",destination = "country.name"))

sim_us_uk$country_name[sim_us_uk$country== "CSK"] <- "Czechoslovakia"
sim_us_uk$country_name[sim_us_uk$country== "YUG"] <- "Yugoslavia"
sim_us_uk$country_name[sim_us_uk$country== "DDR"] <- "East Germany"
sim_us_uk$country_name[sim_us_uk$country== "YMD"] <- "Democratic Yemen"
sim_us_uk$country_name[sim_us_uk$country== "EU"] <- "European Union"


# Similar scores with EU
# empty data frame for similarity scores with EU
sim.eu <- data.frame(matrix(nrow = 0, ncol = 3))

# The EU speeches span from 2011 to 2024
# Indices for those years in the list are 2011-1946+1 = 66 and 
# 2024 - 1946 + 1 = 79
for ( i in 66:79){
  simi <- x[[i]] |> as.matrix() |> as.data.frame()
  countries <- rownames(simi) 
  year <- i + 1945
  simi <- simi |> 
    mutate(country = countries, year = year) |> 
    select(EU, country, year)
  sim.eu <- rbind(sim.eu, simi)
}


#file3 <- paste0(file_path, "df_sim_select", ".RData")
#df_sim_select <- save(sim, file = file3)

#}

#for (i in 1:6) {
#file4 <- paste0(file_path, "df_sim_select",i, ".RData")
#df_sim_select1 <- get(load(file4))
sim_chn_rus$country_name[sim_chn_rus$country=="RUS"] <- "Russia/Soviet Union"

rus_chn <- sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "RUS") |> 
  mutate(simil = CHN)

usa_chn <- sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "USA") |> 
  mutate(simil = CHN)

vnm_chn <- sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "VNM") |> 
  mutate(simil = CHN)

jpn_chn <-sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "JPN") |> 
  mutate(simil = CHN)

ind_chn <- sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "IND") |> 
  mutate(simil = CHN)

gbr_chn <- sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "GBR") |> 
  mutate(simil = CHN)

eu_chn <- sim_chn_rus |> 
  select(CHN, country, country_name, year) |> 
  filter(country == "EU") |> 
  mutate(simil = CHN)

chn_avg <- sim_chn_rus |> 
  select(CHN, country, year) |> 
  filter(CHN != 1) |> 
  group_by(year) |> 
  summarize(simil = mean(CHN)) |> 
  mutate(country = "China")

usa_avg <- sim_us_uk |> 
  select(USA, country, year) |> 
  filter(USA != 1) |> 
  group_by(year) |> 
  summarize(simil = mean(USA)) |> 
  mutate(country = "United States")

rus_avg <- sim_chn_rus |> 
  select(RUS, country, year) |> 
  filter(RUS != 1) |> 
  group_by(year) |> 
  summarize(simil = mean(RUS)) |> 
  mutate(country = "Russia/Soviet Union")

gbr_avg <- sim_us_uk |> 
  select(GBR, country, year) |> 
  filter(GBR != 1) |> 
  group_by(year) |> 
  summarize(simil = mean(GBR)) |> 
  mutate(country = "United Kingdom")

eu_avg <- sim.eu |> 
  select(EU, country, year) |> 
  filter(EU != 1) |> 
  group_by(year) |> 
  summarize(simil = mean(EU)) |> 
  mutate(country = "European Union")


avg_simil <- rbind(chn_avg, usa_avg, rus_avg, gbr_avg, eu_avg)

chn_simil <- rbind(rus_chn, usa_chn, vnm_chn, 
                   jpn_chn, ind_chn, gbr_chn, eu_chn) |> 
  select(-CHN) 


#avg_simil <- bind_rows(chn_avg, rus_avg, usa_avg, gbr_avg)
 # Temporal trends for similarity scores with China
gg.chn <- ggplot(chn_simil, aes(x = year, y = simil, color = country_name,
                                       linetype = country_name)) +
  geom_smooth(method = "loess", se = F, size = 1.2) + 
  labs(title = "Similarity Scores with China", 
       x = "Year", y = "Similarity Score")

ggsave(filename = paste0(file_path,y, 
                         "_similarity_with_china.pdf"), plot = gg.chn)

# Temporal trends for global average similarity scores with some countries
gg.avg <- ggplot(avg_simil, aes(x = year, y = simil, color = country,
                                linetype = country)) +
  geom_smooth(method = "loess", se = F, size = 1.2) +
  labs(title = "Global Average Similarity Scores with Different Countries",
       x = "Year", y = "Similarity Score")

ggsave(file = paste0(file_path, y,
                     "_global_avg_simil.pdf"),plot = gg.avg)

}

