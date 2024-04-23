install.packages("tidyverse")
installed.packages("plotly")


library(tidyverse)
library(plotly)

unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")


#data joining 1
data_join <- full_join(unicef_indicator_2, unicef_metadata)

# Final data
data_join <- unicef_indicator_2 %>%
  full_join(unicef_metadata)

# World map showing countries receiving ACT
map_world <- map_data("world")                        

#map 1
map_data_join <- full_join(unicef_indicator_2, map_world, by = c ("country" = "region"))

ggplot(map_data_join)+
  aes(x = long, y = lat, group = group, fill = indicator)+
  geom_polygon()+
  scale_fill_discrete()+
  labs(title = "The spread of ACT distribution across the world map",
       subtitle = "Countries in black are not distributing the drug",
       x = "longitude",
       y = "latitude",
       fill = "ACT distribution")+
  theme_classic()+
  theme(text = element_text(family = "serif"))


options(scipen = 999)

#time series showing the evolution of life expectancy across the world
timeseries_plot_1 <- data_join %>%  
  ggplot() +
    aes(year, `Life expectancy at birth`, colour = `country`) +
    geom_line(show.legend = FALSE)+
   labs(
     x = "Year", 
     y = "life expectancy",
     title = "evolution of life expectancy across the world")+
  theme_classic()+
  theme(text = element_text(family = "serif"))

ggplotly(timeseries_plot_1)


#scatter plot 2005-2024 Data filter
filtered_data <- data_join %>%
  filter(year >= 2005 & year <= 2024)


#scatter plot filter 2005-2024 ACT countries
countries_unicef <- unique(unicef_indicator_2$country)

filtered_data_combined <- filtered_data %>%
  filter(year >= 2005 & year <= 2024,
         country %in% countries_unicef)

#Scatterplot showing the Evolution of the relationship between GDP per capita and %of children receiving ACT  
ggplot(filtered_data_combined) +
  aes(`GDP per capita (constant 2015 US$)`, `% of vaccinated children`, color = country, size = `Life expectancy at birth`) +
  geom_point(alpha= 0.2)+
  facet_wrap(~ year, nrow = 1)+
  scale_x_continuous(
    limits= c(0, 7500),
    breaks = c(2500, 5000),
    labels = scales::unit_format(unit = "K", scale = 0.001))+
  labs(
    x = "GDP per capita in US$", 
    y = "% of children receivng ACT",
    title = "Evolution of the relationship between GDP per capita and % of children receivng ACT"
  )+
  guides(color = "none" , size = "none")+
  theme_classic()+
  theme(text = element_text(family = "serif"))
#some countries are not displayed in the plot

#Bar chart
filtered_data_combined %>%
   group_by(sex, year) %>%
 summarise(m_mean_vaccination = mean(`% of vaccinated children`, na.rm = TRUE)) %>%
  ggplot()+
  aes(sex, m_mean_vaccination, fill = sex)+
  geom_col() +
  facet_wrap(~ year, nrow = 1)+
labs(
  x = "", 
  y = "% of children receivng ACT",
  title = "The comparison of males and females receiving ACT over the years", 
  fill = "Sex")+
  theme_classic()+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_blank())+
  scale_fill_manual(values = c("pink", "blue", "green"))



