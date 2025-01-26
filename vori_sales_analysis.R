library(dplyr)
library(fpp3)
library(ggplot2)

getwd()

setwd('/home/mofongo/Documents/ghfc/salesAnalysis')
list.files()

vori <- read.csv('./vori_sales_data/vori_sales_09122023_12262024.csv')
vori.2 <- read.csv('./vori_sales_data/vori_Net Sales_first6months.csv')
vori.3 <- read.csv('./vori_sales_data/voriSales_2H23_testNet Sales.csv')

# normalize the names of the dataframes, then compare their column set and schema
names(vori) <- gsub('\\.','_',names(vori))
names(vori.2) <- gsub('\\.','_',names(vori.2))
names(vori.3) <- gsub('\\.','_',names(vori.3))

#compare equality
all(names(vori) == names(vori.2)) && all(sapply(vori, class) == sapply(vori.2, class))

# Define the function
normalize_df <- function(df) {
  #ensure that the df contains an Order_Day and Value field
  if(!all(list('Order_Day','Value') %in% names(df))) {
    stop("expected column names are not found in the passed DF")
  }
  
  # Main logic
  df |>
    mutate(date = as.Date(df$Order_Day,'%a, %b %d, %Y')) -> df.1
  
  #isolate the only columns I care about
  df.1 |>
    select(date,Value) |>
    unique() -> sales.vori
  
  #output 
  sales.vori |>
    as_tsibble(index = date) -> vori.t
  
  if(!nrow(vori.t)>0){
    stop("transformed df has 0 rows")
  }
  
  # Return the result
  return(vori.t)
}

test <- normalize_df(vori)
test.2 <- normalize_df(vori.2)
test.3 <- normalize_df(vori.3)


#check min and max dates
test.2 |>
  as.data.frame() |>
  summarise(min = min(date), max = max(date))

#reveal the interval
interval(test.2)

#fill gaps, setting them to 0
test |>
  fill_gaps(Value = 0) -> test.ng

test.2 |>
  fill_gaps(Value = 0) -> test.2.ng

test.3 |>
  fill_gaps(Value = 0) -> test.3.ng

#change the grouping to monthly
test.ng |>
  index_by(yearmon = yearmonth(date)) |>
  summarize(size = n(), sales = sum(Value)) -> test.ng.2

test.2.ng |>
  index_by(yearmon = yearmonth(date)) |>
  summarize(size = n(), sales = sum(Value)) -> test.2.ng.2

test.3.ng |>
  index_by(yearmon = yearmonth(date)) |>
  summarize(size = n(), sales = sum(Value)) -> test.3.ng.2


#verify interval
interval(test.2.ng.2)


test.ng.2 |>
  filter(size>27) |>
  autoplot(vars(sales))

test.3.ng.2 |>
  filter(size>27) |>
  autoplot(vars(sales))



names(test.ng.2)

#build the appropriate factor variable
test.ng.2 |>
  mutate(yr_factor = factor(year(yearmon), labels = c("2023","2024"))) -> plot.df

add_commas <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

plot.df |>
  filter(size>27) |>
  #mutate(yr_factor = as.factor(year(yearmon)) |>
  ggplot(aes(x=month(yearmon), color = yr_factor, group = yr_factor)) +
  geom_line(aes(y=sales)) +
  geom_point(aes(y=sales)) +
  scale_y_continuous(label = add_commas) +
  scale_x_continuous(
    breaks = 1:12,  # Specify the positions for ticks
    labels = month.abb  # Replace with full month names
  ) 


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
