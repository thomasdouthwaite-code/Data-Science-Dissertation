library(dplyr)

x <- arrivals_data_per_resource %>% 
  filter(resource == "monthly statement") %>% 
  filter(name == "Basic Customer1")

barplot(diff(x$end_time))

y <- arrivals_data_per_resource %>% 
  filter(resource == "monthly statement") %>% 
  filter(name == "Basic Customer0")

barplot(diff(y$end_time))

mean(diff(x$end_time) , na.rm = TRUE)

z <- touchpoints %>%
  filter(key == "ATM notification") %>%  
  filter(time > 350)
mean(z$value)

journey <- left_join(touchpoints, arrivals_data, by = "name")

journey$relative_time <- journey$time - journey$start_time

ggplot(journey, aes(x = relative_time, y = value)) +
  geom_line( aes(colour = key), size = 1) +
  geom_smooth(method = "lm") + facet_grid(. ~ key)

ggplot(journey, aes(x = relative_time, y = value)) +
  geom_point( aes(colour = key), size = 1) +
  geom_jitter(aes(colour = key), size = 1)+ 
  geom_smooth(method = "lm") + facet_grid(. ~ key)


skinny_journey <- journey

skinny_journey <- subset(skinny_journey, select = -c(replication.y, replication.x, 
                                                         finished, activity_time))




grouped <- journey %>%  group_by(relative_time, key) %>%  summarise(avg = mean(value))

ggplot(grouped, aes(x = relative_time, y = avg)) +
  geom_line( aes(colour = "Average Customer Journey"), size = 1) + facet_grid(. ~ key)

channel_graph <- ggplot(grouped, aes(x = relative_time, y = avg, fill = key)) +
                 geom_area(position = "fill") + xlab("Customer Journey Time (Days)") +
                 ylab("Proportion of Total Touchpoints") + theme_classic()
channel_graph + labs(fill = "Channel")

ggplot(grouped, aes(x = relative_time, y = avg)) +
 geom_line( aes(colour = key), size = 1) +
  geom_smooth(aes(colour = "Average Customer"),
              method = "lm", size = 0.5) + facet_grid(. ~ key)+
  scale_colour_manual(values=c("red","black","blue", "green"))

