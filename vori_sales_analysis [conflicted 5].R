library(dplyr)
library(fpp3)

getwd()

setwd('/home/mofongo/Documents/ghfc/salesAnalysis')
list.files()

vori <- read.csv('./vori_sales_09122023_12262024.csv')
summary(vori)
names(vori)
str(vori)

names(vori) <- gsub('\\.','_',names(vori))

vori |>
  mutate(date = as.Date(vori$Order_Day,'%a, %b %d, %Y')) -> vori

#isolate the only columns I care about
vori |>
  select(date,Value) |>
  unique() -> sales.vori

#check min and max dates
sales.vori |>
  summarize(min = min(date), max = max(date))

sales.vori |>
  as_tsibble(index = date) -> vori.t

#reveal the interval
interval(vori.t)

class(vori.t)

vori.t |>
  autoplot(Value)

scan_gaps(vori.t.ng)

#fill gaps, setting them to 0
vori.t |>
  fill_gaps(Value = 0) -> vori.t.ng

#change the grouping to monthly
vori.t.ng |>
  index_by(yearmon = yearmonth(date)) |>
  summarize(size = n(), sales = sum(Value)) -> vori.t.ng

interval(vori.t.ng)

vori.t |>
ggplot(aes(x = date)) +
  geom_line(aes(y = Value)) +
  labs(title = 'Vori Daily Sales', x = 'day', y = 'sales')

vori.t.ng |>
  ggplot(aes(x=yearmon)) +
  geom_line(aes(y=sales))

vori.t.ng |>
  filter(size > 27) |>
  autoplot(sales) +
  labs(title = 'Monthly Sales for Vori')

vori.t.ng |>
  filter(size > 27) |>
  gg_season(sales, labels = 'right') +
  labs(title = 'Monthly Sales Vori', x = 'month-yr')

vori.t.ng |>
  filter(month(yearmon) %in% c(10,11,12))
  
month(vori.t.ng$yearmon)
