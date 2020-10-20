###############################################################################
# Helper functions for the SMDA app
#
# Author: Andrew Smith
# Created 2020-10-02
###############################################################################

# Get our picks
getPicksData <- function(){

  picks = read.csv(url("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBC7iMqU4JNUBNqzmxPt7o_blG9ritXAR0RAQmbAUFaHO4oQpnWn8cKrj9tXSxerlwc5fAIbuOxiLH/pub?gid=343251432&single=true&output=csv"))
  
  return(picks)
}

# Takes a single company and gets the share prices between two dates
getSharePrices <- function(pick){
  
  if(is.na(pick$date_sold)){pick$date_sold = today()}
  share_prices = data.frame(getSymbols(pick$ticker, from = pick$date_picked, to = pick$date_sold, auto.assign = FALSE, warnings = TRUE))
  share_prices <- tibble::rownames_to_column(share_prices, "date")
  share_prices <- share_prices %>% mutate(date = gsub("X", "", date))
  share_prices <- share_prices[,c(1,2)]
  colnames(share_prices) <- c("date","share_price")
  
  # Merging with other details
  share_prices <- merge(pick, share_prices)
  share_prices <- share_prices %>% 
    mutate(date = ymd(date)) %>%
    filter(!is.na(date))
  
  # How many shares did we purchase
  start_price <- share_prices %>% filter(date == date_picked) %>% select(share_price) %>% as.numeric()
  share_prices <- share_prices %>%
    mutate(start_price = start_price) %>%
    mutate(num_shares = stake/start_price) %>%
    mutate(capital = num_shares * share_price)
  
  return(share_prices)
}

runPriceFetcher <- function(){
  picks <- picks %>% mutate(date_picked = dmy(date_picked),
                            date_sold = dmy(date_sold))
  
  all_prices = data.frame()
  for (i in 1:nrow(picks)){
    pick = picks %>% slice(i)
    prices = getSharePrices(pick)
    all_prices = rbind.data.frame(all_prices, prices)
  }
  
  all_prices <- all_prices %>% filter(!is.na(share_price))
  return(all_prices) 
}

