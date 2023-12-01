# Turkish Option Analysis
 We try to examine performance of various option strategies in Turkish market

In the Borsa Istanbul all option contracts are European contracts.

## Data

Data obtained from monthly dayend  price and volume report. It contains Contract name,date and settlement price. Other critical variables like Underlying asset, settlement date, strike price, option type obtained via contract name.

## Analysis

Implied Volatility and Greeks calculated using Black-Scholes model.

In equity contracts, as risk free rate, 1,3,6 month deposit rates are used. In this analysis hold the maturity is the main strategy so for each contract closest rate to maturity is chosen as risk-free rate. 

Underlying asset prices used from Yahoo Finance.

Next step is checking selected option strategies. 
