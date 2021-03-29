
# Loading configurations ---------------------------------------------------


install.packages("pacman")
pacman::p_load(rvest,
               tidyverse,
               dplyr,
               janitor)

# Scraping tickers from wikipedia -----------------------------------------

sp500url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp500 <- read_html(sp500url) %>% 
  html_node("table") %>% 
  html_table()


# Renaming columns and subsetting -----------------------------------------

sp500 <- sp500 %>% 
  janitor::clean_names() %>% 
  select(-c(sec_filings,date_first_added,cik,founded))


# Specifying column names -------------------------------------------------

names(sp500) <- c("Ticker","Name","Sector","Industry","hqLocation")


# Saving file -------------------------------------------------------------

save(sp500, file="sp500.RData")


# Scraping historical price data ------------------------------------------
aapl <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1301270400&period2=1616889600&interval=1d&events=history&includeAdjustedClose=true")

## This is going to give us some error ------------------------------------
BRK.B <- try(read_csv("https://query1.finance.yahoo.com/v7/finance/download/BRK.B?period1=1540944000&period2=1604102400&interval=1d&events=history&includeAdjustedClose=true"))
mode(BRK.B)


## Creating empty df -------------------------------------------------------

returns <- as.data.frame(matrix(NA, ncol=8, nrow=0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "AdjClose", "Volume", "Ticker")

for(symbol in sp500$Ticker){
  print(symbol)
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol, "?period1=1301270400&period2=1616889600&interval=1d&events=history&includeAdjustedClose=true")
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


# Some manipulation -------------------------------------------------------

returns <- returns %>% 
  mutate(Movement = ifelse((Close>Open), "Up", "Down"))


save(returns, file = "returns.RData")


# Reshaping the df --------------------------------------------------------

returnsLong <- returns %>% 
  gather("Series", "Value", -Date, -Ticker, -Movement)

returnsLong <- returnsLong %>% left_join(sp500 %>% select(Ticker, Name, Sector, Industry), by = c("Ticker"="Ticker"))


# Saving the file ---------------------------------------------------------

save(returnsLong, file ="returnsLong.RData")









