library(tidyverse)
library(quantmod)

# Load Data
getSymbols("GOOG",src="yahoo")
getSymbols("DEXJPUS",src="FRED")
getSymbols("XPT/USD", src="oanda")

# Charting
barChart(GOOG["2020/"])
lineChart(GOOG)
candleChart(GOOG["2020-09-30/"], multi.col=TRUE)

chartSeries(XPTUSD, name="Platinum (.oz) in $USD") 
chartSeries(to.weekly(XPTUSD),up.col='white',dn.col='blue')

# Technical Analysis
chartSeries(GOOG["2020-09-30/"]) 
addMACD()
addBBands()

# OHLC Basics 
Op(GOOG)
Hi(GOOG)
Lo(GOOG)
Cl(GOOG)
Vo(GOOG)
Ad(GOOG)

# Check Availability
is.OHLC(GOOG)
has.OHLC(GOOG)
has.Op(GOOG)
has.Cl(GOOG)
has.Hi(GOOG)
has.Lo(GOOG)
has.Ad(GOOG)
has.Vo(GOOG)
seriesHi(GOOG)
seriesLo(GOOG)

# Percent change
OpCl(GOOG)
OpOp(GOOG)
OpLo(GOOG)
OpHi(GOOG)
HiCl(GOOG)
LoCl(GOOG)
LoHi(GOOG)
ClCl(GOOG)

# Key Functions
Lag(Op(GOOG), k = 1:3)
Next(Op(GOOG), k = 1)
Delt(Op(GOOG), Op(GOOG), k = 0)
Delt(Op(GOOG), Op(GOOG), k = 1)
Delt(Op(GOOG), Cl(GOOG), k = 1:3)

# Subsetting with xts dates
GOOG["2020"]
GOOG["2020-01"]
GOOG["2020-01::2020-12"]
GOOG["::"]
GOOG["2020::"]
GOOG["2020/"]
GOOG[c("2020-01-02", "2020-12-31")]

# Subsetting with first and last
last(GOOG)
last(GOOG, 10)
last(GOOG, "3 weeks")
last(GOOG, "-3 weeks")
last(GOOG, "3 months")
last(first(GOOG, "2 weeks"), "3 days")

# Aggregating Different Time Scale
periodicity(GOOG)
to.weekly(GOOG)
to.monthly(GOOG)
periodicity(to.monthly(GOOG))
ndays(GOOG)

getFX("USD/EUR")
periodicity(USDEUR)
to.weekly(USDEUR)
periodicity(to.weekly(USDEUR))

# Apply by Period
endpoints(GOOG, on = "months")
GOOG[endpoints(GOOG, on = "months")]

apply.weekly(GOOG, FUN = function(x) { max(Cl(x)) })
period.apply(GOOG, endpoints(GOOG, on = "weeks"), FUN = function(x) { max(Cl(x)) })
period.max(Cl(GOOG), endpoints(GOOG, on = "weeks"))
period.min(Cl(GOOG), endpoints(GOOG, on = "weeks"))
period.sum(OpCl(GOOG), endpoints(GOOG, on = "weeks"))
period.prod(OpCl(GOOG), endpoints(GOOG, on = "weeks"))

# Period Returns
dailyReturn(GOOG)
weeklyReturn(GOOG)
monthlyReturn(GOOG)
yearlyReturn(GOOG)
allReturns(GOOG)

# More charting
barChart(GOOG["2020-09/"], theme = "black.mono", bar.type = "hlc") 
lineChart(GOOG["2020/"], line.type = "h", TA = NULL) 
chartSeries(GOOG)
candleChart(GOOG, subset = "2020-11::2021")
reChart(major.tick = "months", subset = "first 16 weeks")

chartSeries(GOOG, subset = "last 4 months")
reChart(subset = "last 8 months")

# Technical Analysis
# | Indicator                                      | TTR Name | quantmod Name |
# |:----------------------------------------------:|:--------:|:-------------:|
# | Welles Wilder's Directional Movement Indicator | ADX      | addADX        |
# | Average True Range                             | ATR      | addATR        |
# | Bollinger Bands                                | BBands   | addBBands     |
# | Bollinger Band Width                           | N/A      | addBBands     |
# | Bollinger %b                                   | N/A      | addBBands     |
# | Commodity Channel Index                        | CCI      | addCCI        |
# | Chaiken Money Flow                             | CMF      | addCMF        |
# | Chande Momentum Oscillator                     | CMO      | addCMO        |
# | Double Exponential Moving Average              | DEMA     | addDEMA       |
# | Detrended Price Oscillator                     | DPO      | addDPO        |
# | Exponential Moving Average                     | EMA      | addEMA        |
# | Price Envelope                                 | N/A      | addEnvelope   |
# | Exponential Volume Weigthed Moving Average     | EVWMA    | addEVWMA      |
# | Options and Futures Expiration                 | N/A      | addExpiry     |
# | Moving Average Convergence Divergence          | MACD     | addMACD       |
# | Momentum                                       | momentum | addMomentum   |
# | Rate of Change                                 | ROC      | addROC        |
# | Relative Strength Indicator                    | RSI      | addRSI        |
# | Parabolic Stop and Reverse                     | SAR      | addSAR        |
# | Simple Moving Average                          | SMA      | addSMA        |
# | Stocastic Momentum Index                       | SMI      | addSMI        |
# | Triple Smoothed Exponential Oscillator         | TRIX     | addTRIX       |
# | Volume                                         | N/A      | addVo         |
# | Weighted Moving Average                        | WMA      | addWMA        |
# | Williams %R                                    | WPR      | addWPR        |
# | ZLEMA                                          | ZLEMA    | addZLEMA      |

chartSeries(GOOG, TA = NULL) 
chartSeries(GOOG, TA = "addVo(); addBBands(); addCCI()")

chartSeries(GOOG, TA = NULL)
addVo()
addBBands()
addCCI()

chartSeries(GOOG, TA = NULL)
addTA(OpCl(GOOG), col = "blue", type = "h")

addOpCl <- newTA(OpCl, col = 'green', type = 'h')
addOpCl() 
