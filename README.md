# Volatility-Index-Pricing-VIX-index-Building
In this project we investigated how to compute volatility indices, properties of volatility, and how to price derivatives on volatility.

Pricing options requires good estimates of expected volatility. Since 2004 traders can directly get an exposure to volatility by trading futures on so called volatility indices. Volatility indices reflect the expected variance (under the risk-neutral measure).

Downloaded several option series on the SP500 index from OptionMetric (note, that the strike price is multiplied by 1,000). Wrote R program with two parameters:

Interest rate
Days to compute the volatility index

Using these option series and the program, constructed a 30-day and 93-day volatility index as explained in “The CBOE Volatility Index - VIX” (CBOE) official white paper on building vix index. In the construction, we assumed an interest-rate of zero percent and options expire at midnight.

Further downloaded VIX, VXV, and SPY closing prices from Yahoo finance and calculated:

Correlation between 30-day volatility index & actual VIX closing figure
Correlation between 93-day volatility & actual VXV closing figure
Correlation between 30-day volatility index & actual SPY closing price
