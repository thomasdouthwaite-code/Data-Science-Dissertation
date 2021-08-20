# Download Packages
install.packages("simmer.plot")
install.packages("simmer.bricks")

library(simmer)
library(simmer.plot)
library(parallel)
library(simmer.bricks)
library(ggplot2)

# set random seed
set.seed(12)

# define customer journeys

basic <-
  trajectory("Basic Customer's path") %>%
  log_("Open Account") %>%
  seize("New to Bank") %>%
  set_attribute("Posted Mail", 2, mod="+")%>%
  log_("NTB Campaign Begins") %>%
  set_attribute("Emails", 1, mod="+")%>%
  timeout(7) %>%
  rollback( 2, 2) %>%
  log_("NTB Campaign Ends")%>%
  release("New to Bank") %>%
  
  ### Month cycle ###  
  seize("Monthly Statement") %>%
  set_attribute("Posted Mail", 1, mod="+") %>%
  timeout(1) %>%
  release("Monthly Statement") %>%

  branch(option = function() {sample(c(1, 2, 3, 4, 5), size = 1, replace = TRUE, prob = c(0.1, 0.3, 0.3, 0.1, 0.2))},
         continue = c(TRUE, TRUE, TRUE, TRUE, TRUE),
         
         trajectory() %>% timeout(6) %>% 
           seize("Regulatory Change") %>% 
           log_("New Regulations") %>% 
           timeout(1) %>%
           set_attribute("Posted Mail", 1, mod="+") %>% 
           release("Regulatory Change"),
         
         trajectory() %>% timeout(6) %>% 
           seize("Marketing 1") %>% 
           log_("New Savings Account Available") %>% 
           timeout(1) %>%
           set_attribute("ATM Notification", 1, mod="+") %>% 
           release("Marketing 1"),
         
         trajectory() %>% timeout(6) %>% 
           seize("Marketing 2") %>% 
           log_("New Credit Card Available") %>% 
           timeout(1) %>%
           set_attribute("ATM Notification", 1, mod="+") %>% 
           release("Marketing 2"),
         
         trajectory() %>% timeout(6) %>% 
           seize("Data Capture") %>% 
           log_("Data Capture") %>% 
           timeout(1) %>%
           set_attribute("Emails", 1, mod="+") %>% 
           release("Data Capture"),
         
         trajectory() %>% timeout(7)
         
  )%>%
  rollback( 1, 3) %>% 
  rollback( 6, 11) %>%
  log_("End of First Year")

#Set number of customers - 500 basic customers over 365 time units

b500 <- runif(500, 0, 365)
b500 <- b500 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

bank <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b500, -1)}, mon = 2) #%>%


#run simulation 
?add_generator()
bank %>% run(until = Inf)

bank %>% get_mon_arrivals(per_resource = TRUE, ongoing = FALSE)


###############################################################################################
#analyse

# Collect Simulation Data
arrivals_data <- bank %>% get_mon_arrivals(per_resource = FALSE, ongoing = TRUE)

arrivals_data_per_resource <- bank %>% get_mon_arrivals(per_resource = TRUE, ongoing = TRUE)

campaigns <- get_mon_resources(bank)

touchpoints <- bank %>% get_mon_attributes()

#### Create Visualisations

# visualise customers

plot(arrivals_data, metric = "flow_time")
plot(arrivals_data, metric = "activity_time")


plot(arrivals_data_per_resource, metric = "flow_time")
plot(arrivals_data_per_resource, metric = "activity_time")


#visualise campaigns

plot(campaigns, metric = "usage", c("New to Bank", "Monthly Statement",
                                    "Regulatory Change", "Marketing 1",
                                    "Marketing 2", "Data Capture"), items = "server", steps = TRUE)

Campaign_Usage <- ggplot(campaigns, aes(x = time, y = server)) +
  geom_line( aes(colour = resource), size = 1) + facet_grid(. ~ resource) + ylab("Resource Usage") +
  xlab("Simulation Time (Days)")
Campaign_Usage

campaigns_w <- campaigns
campaigns_w$week <- floor(campaigns_w$time / 7) 
campaigns_w$month <- floor(campaigns_w$time / 28) 

ggplot(campaigns_w, aes(x = week, y = server)) +
  geom_line( aes(colour = resource), size = 1) + facet_grid(. ~ resource)

ggplot(campaigns_w, aes(x = month, y = server)) +
  geom_line( aes(colour = resource), size = 1) + facet_grid(. ~ resource)

ggplot(subset(campaigns, resource %in% c("New to Bank")), aes(x = time, y = server)) +
  geom_line(size = 1.5)

skinny_campaigns <- campaigns

skinny_campaigns <- subset(skinny_campaigns, select = -c(capacity, queue_size, 
         system, limit, replication))



# visualise touchpoints
plot(touchpoints)

z <- touchpoints
z$time <- floor(touchpoints_w$time / 7)
plot(z)


ggplot(touchpoints, aes(x = time, y = value)) +
  geom_line( aes(colour = key), size = 1) + facet_grid(. ~ key)

ggplot(touchpoints, aes(x = time, y = value, fill = key)) +
  geom_area(position = "stack")

# Add weeks
touchpoints_w <- touchpoints
touchpoints_w$week <- floor(touchpoints_w$time / 7) 

ggplot(touchpoints_w, aes(x = week, y = value)) +
  geom_line( aes(colour = key), size = 1) + facet_grid(. ~ key)



# ggplot(touchpoints, aes(x = time, y = value)) +
#   geom_line( aes(colour = name), size = 1) + facet_grid(. ~ name)

