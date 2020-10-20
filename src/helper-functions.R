###############################################################################
# Helper functions for the SMDA app
#
# Author: Andrew Smith
# Created 2020-10-02
###############################################################################

# Get our picks
getPicksData <- function(){

  sheet = getForm("https://spreadsheets.google.com/spreadsheet/pub", 
                  hl ="en_US", 
                  key = "2PACX-1vQBC7iMqU4JNUBNqzmxPt7o_blG9ritXAR0RAQmbAUFaHO4oQpnWn8cKrj9tXSxerlwc5fAIbuOxiLH", 
                  output = "csv", 
                  .opts = list(followlocation = TRUE, 
                               verbose = TRUE, 
                               ssl.verifypeer = FALSE))
  
  df <- read.csv(textConnection(sheet))
  
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQBC7iMqU4JNUBNqzmxPt7o_blG9ritXAR0RAQmbAUFaHO4oQpnWn8cKrj9tXSxerlwc5fAIbuOxiLH/pubhtml?gid=343251432&single=true"
  
}

# Takes a single company and gets the share prices between two dates
getSharePrices <- function(pick){
  
  if(is.na(pick$date_sold)){pick$date_sold = today()}
  share_prices = data.frame(getSymbols(pick$ticker, from = pick$date_picked, to = pick$date_sold, auto.assign = FALSE))
  share_prices <- tibble::rownames_to_column(share_prices, "date")
  share_prices <- share_prices[,c(1,2)]
  colnames(share_prices) <- c("date","share_price")
  
  # Merging with other details
  share_prices <- merge(pick, share_prices)
  share_prices <- share_prices %>% 
    mutate(date = ymd(date))
  
  # How many shares did we purchase
  start_price <- share_prices %>% filter(date == date_picked) %>% select(share_price) %>% as.numeric()
  share_prices <- share_prices %>%
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
  
  return(all_prices) 
}

