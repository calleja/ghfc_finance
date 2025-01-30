## code to consolidate all the spreadsheets from Revel##
## all sales attachments are eventually concatenated into one
## code could include the 2023 emails from Ray and all prior spreadsheets
library(readxl)
library(dplyr)
library(fpp3)
library(stringr)

#import and consolidate all spreadsheets from 2023
setwd('/home/mofongo/Documents/ghfc/salesAnalysis/rawRevelExports/revelSalesFiles')

#some testing... safe to IGNORE
list.files()
grep(".",list.files('/home/mofongo/Documents/ghfc/salesAnalysis/rawRevelExports'), value = TRUE)
test<-readxl::read_xlsx('./attachment_1.xlsx')

#test whether all attachments carry the same field name list
accum_fieldnames <- function(filepath,pattern='.') {
  accum_fieldname <- list() #store a list of lists
  #set filepath to current wd
  for (i in grep(paste0('^',pattern),list.files(filepath), value = TRUE)){
    # handle xlsx and csv
    if(str_detect(i,"csv")){
      case <- read.csv(paste0(filepath,'/',i))
    }
    else {
      case <- readxl::read_xlsx(paste0(filepath,'/',i))
    }
    
    accum_fieldname <- append(accum_fieldname,list(names(case)))
  }
    
  if(!length(accum_fieldname) > 0){
    stop("list is of length 0")
  }
    
  return(accum_fieldname)  
  }

#list of lists  
test_results <- accum_fieldnames('/home/mofongo/Documents/ghfc/salesAnalysis/rawRevelExports/revelSalesFiles')
yearly_files <- accum_fieldnames('/home/mofongo/Documents/ghfc/salesAnalysis/rawRevelExports','Sales')  

#check whether the contents of each nested list is the same
setequal(test_results[1][[1]],test_results[2][[1]])
test_results[1][[1]]
#yearly sales files have completely different fieldnames
#fieldname options: Time.From, Gross.Sales, Net.Sales
yearly_files[2][[1]]

check_list <- function(lista) {
  size <- length(lista)
  i = 1
  while (i+1 <= size) {
    print(paste0('pair set ',i, 'has equal value of ',setequal(lista[i][[1]],lista[i+1][[1]])))
    i = i+1
  }
}

#proves that field name set is not consistent
check_list(test_results)

#will resort to taking out the fields I care about: Date, Net Sales, Total Payments
accum_df <- function(filepath) {
  accum_df_ls <- list() #store a list of lists
  #set filepath to current wd
  for (i in list.files(filepath)) {
    case <- readxl::read_xlsx(paste0('./',i))
    #print(case[1:5,c('Date', 'Net Sales', 'Total Payments')])
    accum_df_ls <- append(accum_df_ls,list(case[,c('Date', 'Net Sales', 'Total Payments')]))
  }
  
  if(!length(accum_df_ls) > 0){
    stop("list is of length 0")
  }
  
  return(accum_df_ls)  
}

#concatenate all df into a list
all_df <- accum_df('/home/mofongo/Documents/ghfc/salesAnalysis/rawRevelExports/revelSalesFiles')

#all entries from Ray's email attachments in one dataframe; 296 records, so there are substantial records missing
combined_df <- bind_rows(all_df)

head(combined_df)

str(combined_df)

combined_df |>
  filter(Date != 'Totals') |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
  group_by(Date) |>
  summarize(net.sales = max(`Net Sales`), total_pmt = max(`Total Payments`)) -> combined_df.1


#exposes overlap in sales with Vori <- those dates will need to be summed in order to accurately measure total sales
combined_df.1 |>
  summarize(min = min(Date), max = max(Date))

#identify gaps in Revel data after converting the dataframe to a tsibble
combined_df.1 |>
  as_tsibble(index = Date) -> combined_ts

#data contains gaps for 7 days in June and 7 days in July; ideally would fill these gaps with the values from the previous week
scan_gaps(combined_ts_ng)

#a tsibble with no gaps
combined_ts |>
  fill_gaps() |>   # Ensure all time points are present
  mutate(net.sales = ifelse(is.na(net.sales), lag(net.sales, 7), net.sales)) ->combined_ts_ng
