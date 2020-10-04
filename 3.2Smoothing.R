data("polls_2008")
qplot(day,margin, data = polls_2008)

#### Bin Smoothing and Kernels ####
# bin smoothers
#wiggly due to each time window move, 2 points change
span <- 7
fit <- with(polls_2008,ksmooth(day,margin,x.points = day, kernel = "box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day,margin)) +
  geom_point(size = 3, alpha= .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

# kernel
# weighted average --> more weight on points closer to average
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel = "normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

# Local weighted Regression (loess)
total_days <- diff(range(polls_2008$day))
span <- 21/total_days #the larger the span the smoother

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008) #fitting a line (degree 1)

polls_2008 %>% mutate(smooth=fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

#ggplot smooth uses loess
polls_2008 %>% ggplot(aes(day,margin)) +
  geom_point() +
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))

polls_2008 %>% ggplot(aes(day,margin)) +
  geom_point() +
  geom_smooth()


#### Assessment
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- loess(deaths ~ as.numeric(date), degree = 1, span = span, data = dat) 

dat %>% filter(!is.na(deaths)) %>%
  mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot(aes(date, deaths)) +
  geom_point(size = 1, alpha = 0.5, color = "grey") +
  geom_line(aes(date,smooth), color = "red")
  
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#Q3
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)
mnist_27$train %>%
  mutate(y1 = ifelse(y==7,1,0)) %>%
  ggplot(aes(x_2, y1)) +
  geom_point() +
  geom_smooth()
