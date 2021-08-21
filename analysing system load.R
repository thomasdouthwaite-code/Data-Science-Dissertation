##########
#Analysing System Load
install.packages("microbenchmark")
library(microbenchmark)
?microbenchmark()

microbenchmark(ggplot(x, aes(b500)) + geom_histogram())


citation("microbenchmark")

library(descr)
#record processing time

b500 <- runif(500, 0, 365)
b500 <- b500 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

#500 customers
bank <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b500, -1)}, mon = 2) #%>%
bank %>% run(until = Inf)

bank1 <- microbenchmark(bank %>% run(until = Inf), times = 100)

bank1 <- data.frame(descr(bank1$time))

#750 customers
b750 <- runif(750, 0, 365)
b750 <- b750 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

bank2 <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b750, -1)}, mon = 2) #%>%
bank2 %>% run(until = Inf)

bank2 <- microbenchmark(bank2 %>% run(until = Inf), times = 100)

bank2 <- data.frame(descr(bank2$time))

#1000 customers
b1000 <- runif(1000, 0, 365)
b1000 <- b1000 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

bank3 <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b1000, -1)}, mon = 2) #%>%
bank3 %>% run(until = Inf)

bank3 <- microbenchmark(bank3 %>% run(until = Inf), times = 100)

bank3 <- data.frame(descr(bank3$time))

#1500 customers
b1500 <- runif(1500, 0, 365)
b1500 <- b1500 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

bank4 <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b1500, -1)}, mon = 2) #%>%
bank4 %>% run(until = Inf)

bank4 <- microbenchmark(bank4 %>% run(until = Inf), times = 100)

bank4 <- data.frame(descr(bank4$time))

#2000 customers
b2000 <- runif(2000, 0, 365)
b2000 <- b2000 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

bank5 <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b2000, -1)}, mon = 2) #%>%
bank5 %>% run(until = Inf)

bank5 <- microbenchmark(bank5 %>% run(until = Inf), times = 100)

bank5 <- data.frame(descr(bank5$time))

#2500 customers
b2500 <- runif(2500, 0, 365)
b2500 <- b2500 %>% 
  as.numeric() %>% 
  floor() %>% 
  sort(decreasing = FALSE) %>% 
  diff()

bank6 <-
  simmer("bank") %>%
  add_resource("New to Bank", Inf) %>%
  add_resource("Monthly Statement", Inf) %>%
  add_resource("Regulatory Change", Inf) %>%
  add_resource("Marketing 1", Inf) %>%
  add_resource("Marketing 2", Inf) %>%
  add_resource("Data Capture", Inf) %>%
  add_generator("Basic Customer", basic, function() {c(0, b2500, -1)}, mon = 2) #%>%
bank6 %>% run(until = Inf)

bank6 <- microbenchmark(bank6 %>% run(until = Inf), times = 100)

bank6 <- data.frame(descr(bank6$time))

##################

t <- as.numeric(c(371151, 546129, 396786, 482132))
tm <- t/1000000
customers <- as.numeric(c(500, 1000, 1500, 2000))

scalability <- data.frame(customers, t, tm)

ggplot(scalability, aes( x= customers, y = tm)) + geom_line()
