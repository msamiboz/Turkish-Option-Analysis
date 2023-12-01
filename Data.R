library(tidyverse)
library(lubridate)
library(zoo)
setwd("~/Desktop/Options/Data")

colnam <- c('TRADE_DATE',	'INSTRUMENT_SERIES',	'MARKET',	'MARKET_SEGMENT',
            'INSTRUMENT_TYPE',	'INSTRUMENT_CLASS',	'UNDERLYING',
            'SETTLEMENT_PRICE',	'PREVIOUS_SETTLEMENT_PRICE',
            'CHANGE_OF_SETTLEMENT_PRICE',	'OPENING_PRICE',	'LOWEST_PRICE',
            'HIGHEST_PRICE',	'CLOSING_PRICE',	'REMAINING_BID',	'REMAINING_ASK',
            'VWAP',	'TRADED_VALUE',	'PREMIUM_VALUE',	'TRADE_VOLUME',
            'TRADE_COUNT',	'OPEN_POSITION',	'OPEN_POSITION_CHANGE')

csvs <- list.files()
dat <- tibble()
for (c in csvs) {
  d <- read_delim(c,
                    col_types = "Dccccccddddddddddddddddd",skip = 2,col_names = colnam,
                    delim=";")
  dat <- rbind(dat,d)
}


dat_eq <- dat %>% filter(startsWith(.$INSTRUMENT_SERIES,"O"),MARKET=="D_EQ") %>%
  mutate(under=str_length(UNDERLYING)+1,
        year_mon=substr(INSTRUMENT_SERIES,under+1,under+4),year_mon=as.yearmon(my(year_mon)),
         call_put=ifelse(INSTRUMENT_TYPE=="D_EQ_ECP","call","put"),
         strike_price=as.double(substr(INSTRUMENT_SERIES,under+6,str_length(INSTRUMENT_SERIES)))) %>% 
  select(TRADE_DATE,INSTRUMENT_SERIES,UNDERLYING,SETTLEMENT_PRICE,year_mon,call_put,strike_price) %>%
  na.omit()


#settlement date
x <- seq(as.Date("2007-12-31"), by="1 day", length.out=(as.Date("2027-12-31") - as.Date("2007-12-31")))
df <- data.frame(date=x, year=year(x), month=month(x))
df[,"weekday"] <- weekdays(df[,"date"])
df<- df[! df[,"weekday"] %in% c("Saturday", "Sunday"),]
last_weekday  <- as_tibble(df) %>% group_by(year,month) %>% 
  summarise(last_weekday=max(date),.groups = "drop") %>%
  mutate(year_mon=as.yearmon(ym(paste0(year,"_",month)))) %>%
  select(last_weekday,year_mon)

dat_eq <- left_join(dat_eq,last_weekday,by="year_mon") %>%
  rename(settlement_date=last_weekday) %>% select(-year_mon)

#underlying asset price
library(quantmod)
dat_eq %>% distinct(UNDERLYING) %>%
  mutate(yahoo=gsub("\\.E$", ".IS", UNDERLYING)) %>% pull(yahoo) -> tickers
time_window <- dat_eq %>% distinct(TRADE_DATE) %>% arrange %>% rename(Date=TRADE_DATE)
sd_window <- 300
res <- tibble(Date=as.Date(character()),Stat=character(),value=numeric(),Ticker=character())

for (symbol in tickers) {
  getSymbols(symbol)
  dat <- as_tibble(fortify.zoo(Cl(get(symbol))))
  colnames(dat) <- c("Date","Price")
  dat <- dat %>% mutate(logreturn= log(dplyr::lag(Price,1)) - log(Price),
                        Vol=c(rep(NA,sd_window-1),
                       rollapplyr(logreturn,width=sd_window,FUN=sd))*sqrt(250)) %>%
    select(-logreturn) %>% pivot_longer(cols=-Date,names_to = "Stat") %>%
    mutate(Ticker=symbol)
  dat2 <- time_window %>%
    left_join(.,
              dat, by = "Date")
  res <- rbind(res,dat2)
  rm(list = symbol)
  rm(dat,dat2)
}

prices_tibble <- res %>% na.omit %>% rename(TRADE_DATE=Date,UNDERLYING=Ticker) %>% 
  mutate(UNDERLYING= gsub("\\.IS$", ".E", UNDERLYING),.keep="unused") %>% 
  pivot_wider(names_from = Stat,values_from = value)
  

dat_eq <- dat_eq %>% left_join(.,prices_tibble,by=c("TRADE_DATE","UNDERLYING")) %>%
  mutate(ttm= time_length(difftime(dat_eq$settlement_date, dat_eq$TRADE_DATE), "years")) %>% 
  rename(Underlying_price=Price) %>% na.omit()

#Riskfree rate
#Deposit rate is used for equities
# To connect electronic data distribution system of CBRT, I used algopoly/EVDS package
library(EVDS)
key <- read.table("~/Desktop/Options/EVDS_key.txt") %>% as.character()
set_evds_key(key)
depositrates <- get_series(c("TP.TRY.MT01","TP.TRY.MT02","TP.TRY.MT03"),
                           start_date = "01-01-2007",end_date = "31-12-2027")$items %>% as_tibble() %>% 
  mutate(Date=as.Date(Tarih,format="%d-%m-%Y"),.keep="unused",.before=Tarih) %>% 
  select(-YEARWEEK)
colnames(depositrates) <- c("Date","rf_1m","rf_3m","rf_6m","delete")
depositrates <- depositrates %>% select(-delete) %>% mutate(rf_1m=as.numeric(rf_1m),
                                            rf_3m=as.numeric(rf_3m),
                                            rf_6m=as.numeric(rf_6m))
depositrates_z <- zoo(depositrates[,-1],order.by=depositrates[,1] %>% pull)
depositrates <- merge(depositrates_z,
      zoo(,seq(start(depositrates_z),dat_eq %>%
                                           slice_max(order_by = TRADE_DATE) %>%
                                           distinct(TRADE_DATE) %>% pull,"days"))) %>%
  na.locf() %>% 
  fortify.zoo() %>% as_tibble()

rf_finder <- function(date,time_to_maturity,df=depositrates){
  index <- abs(c(30/365,60/365,90/365) - time_to_maturity ) %>% which.min() + 1
  rf <- df[df$Index==date,index] %>% pull
  return(rf / 100)
}

dat_eq <- dat_eq %>% mutate(rf=map2_dbl(TRADE_DATE,ttm,
                              ~rf_finder(date=.x,time_to_maturity = .y)))

#calculate Impvol
library(RQuantLib)

#try to eliminate observatios with unrealistic prices
adj_EuropeanOptionImpliedVolatility <- function(type,value,underlying,strike,
                                                riskFreeRate,maturity,dividendYield = 0,volatility){

  if (type=="call") {
    min_bound <- underlying - strike*exp(-riskFreeRate*maturity)
    max_bound <- underlying
  }else{
    min_bound <- strike - underlying
    max_bound <- strike
  }
  
  if (value < min_bound) {
    res <- NA
  }else if (value > max_bound) {
    res <- NA
  }else{
    res <- try(EuropeanOptionImpliedVolatility(type = type,value = value,underlying = underlying,strike = strike,
                                           dividendYield = dividendYield,volatility = volatility,riskFreeRate = riskFreeRate,
                                           maturity=maturity) %>% (function(L) L[[1]] ),TRUE)
  }
  if (class(res) == "try-error") {
    res <- NA
  }
  return(res)
  
  
}




dat_eq <- dat_eq %>%na.omit %>%  mutate(ImpVol=pmap(list(call_put,SETTLEMENT_PRICE,Underlying_price,strike_price,rf,ttm,Vol),
                              ~adj_EuropeanOptionImpliedVolatility(type = ..1,value = ..2,
                                                                   underlying = ..3,strike = ..4,
                                                                   riskFreeRate = ..5,maturity = ..6,
                                                                   dividendYield = 0,volatility = ..7)))%>%
  filter(!is.na(ImpVol)) %>% unnest(ImpVol)

#calculate greeks
library(greeks)
# den <-dat_eq[1,]
# Greeks(initial_price = den$Price,exercise_price = den$strike_price,r = den$rf,time_to_maturity = den$ttm,
#        volatility=den$Vol,dividend_yield = 0,model = "black_scholes",option_type = "European",
#        payoff = den$call_put)


dat_eq <- dat_eq %>% mutate(greeks= pmap(list(Underlying_price,strike_price,rf,ttm,Vol,call_put),
                               ~Greeks(initial_price = ..1,exercise_price = ..2,r = ..3,
                                       time_to_maturity = ..4,volatility = ..5,dividend_yield = 0,
                                       model = "black_scholes",option_type = "European",payoff = ..6))) %>% 
  unnest_wider(greeks)

#calculate return when hold the maturity
dat_eq <- left_join(dat_eq,
          prices_tibble %>% rename(settlement_date=TRADE_DATE) %>%
            select(-Vol),
          by=c("settlement_date","UNDERLYING")) %>%
  mutate(Payoff=ifelse(call_put=="call",Price-strike_price,ifelse(call_put=="put",strike_price-Price,NA))) %>% 
  mutate(Payoff=ifelse(Payoff<0,0,Payoff),
         Return=Payoff-SETTLEMENT_PRICE / Underlying_price)


