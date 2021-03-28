#====================
#Loading libraries
#====================
library(tidyverse)
library(here)
library(rvest)
library(dplyr)

#====================
#WebScrape Tickers
#====================

sp500url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp500 <- read_html(sp500url) %>% 
  html_node("table") %>% 
  html_table()

#=======================
#What's happening above?
#=======================
#read_html() is navigating to the url of the page and it's reading the html 
#of that page. Second function html_node() is finding all the html tags that
#is labelled tables. html_table() is parsing out the html and converting it to
#dataframe

#Renaming columns and subsetting the df with required columns only
sp500 <- sp500 %>% 
  janitor::clean_names() %>% 
  select(-c(sec_filings,date_first_added,cik,founded))

#=======================
#Renaming columns
#=======================
names(sp500) <- c("Ticker","Name","Sector","Industry","hqLocation")

#=======================
#Saving the file
#=======================
save(sp500, file="sp500.RData")


#=======================
#WebScapre Price Data
#=======================

aapl <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/APL?period1=1540944000&period2=1604102400&interval=1d&events=history&includeAdjustedClose=true")
msft <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/MSFT?period1=1540944000&period2=1604102400&interval=1d&events=history&includeAdjustedClose=true")

#This is going to give us some error
BRK.B <- try(read_csv("https://query1.finance.yahoo.com/v7/finance/download/BRK.B?period1=1540944000&period2=1604102400&interval=1d&events=history&includeAdjustedClose=true"))
mode(BRK.B)

#Creating a blank dataframe
returns <- as.data.frame(matrix(NA, ncol=8, nrow=0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "AdjClose", "Volume", "Ticker")

for(symbol in sp500$Ticker){
  print(symbol)
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol, "?period1=1540944000&period2=1604102400&interval=1d&events=history&includeAdjustedClose=true")
  ret <- try(read_csv(url))
  
  if(mode(ret) != "character"){
    ret$Ticker <-  symbol
    returns <- rbind(returns, ret)
  }
}

names(returns) <- c("Date", "Open", "High", "Low", "Close", "AdjClose", "Volume", "Ticker")
returns %>% 
  glimpse()


returns <- returns %>% 
  select("Date", "Ticker", "Open", "High", "Low", "Close")

  
returns <- returns %>% 
  mutate(Open = as.numeric(Open),
         High = as.numeric(High),
         Low = as.numeric(Low),
         Close = as.numeric(Close))

#If stock goes up that day, we put that in "Up" column, if not "Down" column
returns <- returns %>% 
  mutate(Movement = ifelse((Close>Open), "Up", "Down"))


save(returns, file = "returns.RData")

#Re-shape the data.frame
#Basically we are saying keep intact data, ticker and movement
#We are going to collapse Open, High, Low columns into a column called "Series"
#into a column called "Value"
returnsLong <- returns %>% 
  gather("Series", "Value", -Date, -Ticker, -Movement)

returnsLong <- returnsLong %>% left_join(sp500 %>% select(Ticker, Name, Sector, Industry), by = c("Ticker"="Ticker"))

#Saving the file
save(returnsLong, file ="returnsLong.RData")









