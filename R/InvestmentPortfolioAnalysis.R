######################################################
# Investment Portfolio Analysis with R               #
# (c) Diego Fernandez Garcia 2015-2017               #
# www.exfinsis.com                                   #
######################################################

# 1. Investment Portfolio Analysis Data

# 1.1. Load R packages
library("tseries")
library("quantmod")
library("Quandl")
library("PortfolioAnalytics")
library("DEoptim")

# 1.2. Data Reading
data <- read.csv("Investment-Portfolio-Analysis-Data.txt",header=T)
data <- xts(data[,2:30],order.by=as.Date(data[,1]))

# 2. Asset Classes

# 2.1. Cash and Cash Equivalents

# 2.1.1. U.S. Total Money Market
# getSymbols("SHV",src="yahoo",from="2007-01-01",to="2017-01-01")
# cash <- SHV[,6]
cash <- data[,1]
mcash <- monthlyReturn(cash,type="arithmetic")
colnames(mcash) <- "mcash"
table.AnnualizedReturns(mcash)
charts.PerformanceSummary(mcash)

# 2.2. Fixed Income or Bonds

# 2.2.1. U.S. Total Bond Market
# getSymbols("AGG",src="yahoo",from="2007-01-01",to="2017-01-01")
# bonds <- AGG[,6]
bonds <- data[,2]
mbonds <- monthlyReturn(bonds,type="arithmetic")
colnames(mbonds) <- "mbonds"

# 2.2.2. U.S. Short Term Bond Market
# getSymbols("SHY",src="yahoo",from="2007-01-01",to="2017-01-01")
# sbonds <- SHY[,6]
sbonds <- data[,3]
msbonds <- monthlyReturn(sbonds,type="arithmetic")
colnames(msbonds) <- "msbonds"

# 2.2.3. U.S. Long Term Bond Market
# getSymbols("TLT",src="yahoo",from="2007-01-01",to="2017-01-01")
# lbonds <- TLT[,6]
lbonds <- data[,4]
mlbonds <- monthlyReturn(lbonds,type="arithmetic")
colnames(mlbonds) <- "mlbonds"

# 2.2.4. International Total Bond Market
# getSymbols("AUBAX",src="yahoo",from="2007-01-01",to="2017-01-01")
# ibonds <- AUBAX[,6]
ibonds <- data[,5]
mibonds <- monthlyReturn(ibonds,type="arithmetic")
colnames(mibonds) <- "mibonds"

# 2.2.5. U.S. Cash, Cash Equivalents and Fixed Income or Bond Markets Returns Comparison
usbondscomp <- cbind(mcash,msbonds,mbonds,mlbonds)
table.AnnualizedReturns(usbondscomp)
charts.PerformanceSummary(usbondscomp)

# 2.2.6. U.S. and International Total Bond Markets Returns Comparison
bondscomp <- cbind(mbonds,mibonds)
table.AnnualizedReturns(bondscomp)
charts.PerformanceSummary(bondscomp)

# 2.3. Equities or Stocks

# 2.3.1. U.S. Total Stock Market
# getSymbols("VTI",src="yahoo",from="2007-01-01",to="2017-01-01")
# stocks <- VTI[,6]
stocks <- data[,6]
mstocks <- monthlyReturn(stocks,type="arithmetic")
colnames(mstocks) <- "mstocks"

# 2.3.2. U.S. Large Cap Stock Market
# getSymbols("SPY",src="yahoo",from="2007-01-01",to="2017-01-01")
# lstocks <- SPY[,6]
lstocks <- data[,7]
mlstocks <- monthlyReturn(lstocks,type="arithmetic")
colnames(mlstocks) <- "mlstocks"

# 2.3.3. U.S. Small Cap Stock Market
# getSymbols("VB",src="yahoo",from="2007-01-01",to="2017-01-01")
# sstocks <- VB[,6]
sstocks <- data[,8]
msstocks <- monthlyReturn(sstocks,type="arithmetic")
colnames(msstocks) <- "msstocks"

# 2.3.4. U.S. Small Cap Growth Stock Market
# getSymbols("VBK",src="yahoo",from="2007-01-01",to="2017-01-01")
# gstocks <- VBK[,6]
gstocks <- data[,9]
mgstocks <- monthlyReturn(gstocks,type="arithmetic")
colnames(mgstocks) <- "mgstocks"

# 2.3.5. U.S. Small Cap Value Stock Market
# getSymbols("VBR",src="yahoo",from="2007-01-01",to="2017-01-01")
# vstocks <- VBR[,6]
vstocks <- data[,10]
mvstocks <- monthlyReturn(vstocks,type="arithmetic")
colnames(mvstocks) <- "mvstocks"

# 2.3.6. International Total Stock Market
# getSymbols("CWI",src="yahoo",from="2007-01-01",to="2017-01-01")
# istocks <- CWI[,6]
istocks <- data[,11]
mistocks <- monthlyReturn(istocks,type="arithmetic")
colnames(mistocks) <- "mistocks"

# 2.3.7. International Developed Stock Market
# getSymbols("EFA",src="yahoo",from="2007-01-01",to="2017-01-01")
# dstocks <- EFA[,6]
dstocks <- data[,12]
mdstocks <- monthlyReturn(dstocks,type="arithmetic")
colnames(mdstocks) <- "mdstocks"

# 2.3.8. International Emerging Stock Market
# getSymbols("VWO",src="yahoo",from="2007-01-01",to="2017-01-01")
# estocks <- VWO[,6]
estocks <- data[,13]
mestocks <- monthlyReturn(estocks,type="arithmetic")
colnames(mestocks) <- "mestocks"

# 2.3.9. U.S. and International Total Stock Markets Returns Comparison
stockscomp <- cbind(mstocks, mistocks)
table.AnnualizedReturns(stockscomp)
charts.PerformanceSummary(stockscomp)

# 2.3.10. U.S. Market Cap Stock Markets Returns Comparison
sizecomp <- cbind(mlstocks,mstocks,msstocks)
table.AnnualizedReturns(sizecomp)
charts.PerformanceSummary(sizecomp)

# 2.3.11. U.S. Investment Style Stock Markets Returns Comparison
stylecomp <- cbind(msstocks,mvstocks,mgstocks)
table.AnnualizedReturns(stylecomp)
charts.PerformanceSummary(stylecomp)

# 2.3.12. International Stock Markets Returns Comparison
istockscomp <- cbind(mdstocks,mistocks,mestocks)
table.AnnualizedReturns(istockscomp)
charts.PerformanceSummary(istockscomp)

# 2.4. Commodities

# 2.4.1. United States Oil Fund
# getSymbols("USO",src="yahoo",from="2007-01-01",to="2017-01-01")
# oil <- USO[,6]
oil <- data[,14]
moil <- monthlyReturn(oil,type="arithmetic")
colnames(moil) <- "moil"

# 2.4.2. Gold Shares
# getSymbols("GLD",src="yahoo",from="2007-01-01",to="2017-01-01")
# gold <- GLD[,6]
gold <- data[,15]
mgold <- monthlyReturn(gold,type="arithmetic")
colnames(mgold) <- "mgold"

# 2.4.3. Commodities Returns Comparison
commcomp <- cbind(moil,mgold)
table.AnnualizedReturns(commcomp)
charts.PerformanceSummary(commcomp)

# 2.5. Real Estate

# U.S. Real Estate Investment Trust
# getSymbols("IYR",src="yahoo",from="2007-01-01",to="2017-01-01")
# reit <- IYR[,6]
reit <- data[,16]
mreit <- monthlyReturn(reit,type="arithmetic")
colnames(mreit) <- "mreit"
table.AnnualizedReturns(mreit)
charts.PerformanceSummary(mreit)

# 2.6. Currency or Foreign Exchange

# 2.6.1. USD Major Currencies Index
# getSymbols("UUP",src="yahoo",from="2007-01-01",to="2017-01-01")
# usd <- UUP[,6]
usd <- data[,17]
musd <- monthlyReturn(usd,type="arithmetic")
colnames(musd) <- "musd"
table.AnnualizedReturns(musd)
charts.PerformanceSummary(musd)

# 2.7. Main Asset Classes Comparison
assetscomp <- cbind(mbonds,mstocks,moil,mgold,mreit,musd)
table.AnnualizedReturns(assetscomp)
charts.PerformanceSummary(assetscomp)

##########################################

# 3. Returns and Risks
# U.S. Large Cap Stock Market
# (Market: U.S. Total Stock Market, Risk Free Rate: U.S. 1 Month Treasury Bills)

# 3.1. Expected Returns

# 3.1.1. Mean
mmean <- mean(mlstocks)

# 3.1.2. Median
mmed <- median(mlstocks)

# 3.1.4. Expected Returns First Comparison
mretcomp1 <- cbind(mmean,mmed)
mretcomp1

# 3.2. Risk

# 3.2.1. Standard Deviation
msd <- sd(mlstocks)

# 3.2.2. Mean Absolute Deviation
mmad <- MeanAbsoluteDeviation(mlstocks)

# 3.2.4. Risk Metrics First Comparison
mriskcomp1 <- cbind(msd,mmad)
mriskcomp1

# 3.2.5. Implied Volatility
# CBOE VIX Index
# getSymbols("^VIX",src="yahoo",from="2007-01-01",to="2017-01-01")
# vix <- VIX[,4]
vix <- data[,18]
mvix <- to.monthly(vix)
mvixa = mvix[,4]/100
plot(mvixa,main="CBOE Volatility Index VIX")
mvix = mvixa / sqrt(12)
mvmean <- mean(mvix)

# 3.2.6. Risk Metrics Second Comparison
mriskcomp2 <- cbind(msd,mmad,mvmean)
mriskcomp2

# 3.2.7. Normalized Return
plot(mlstocks,main="U.S. Large Cap Stocks Returns")
nlstocks <- (mlstocks-mean(mlstocks))/sd(mlstocks)
plot(nlstocks,main="U.S. Large Cap Stocks Normalized Returns")

# 3.2.8. Geometric Expected Return
mgmean <- exp(log(1+mmean)-(msd^2/(2*(1+mmean)^2)))-1

# 3.2.9. Expected Returns Second Comparison
mretcomp2 <- cbind(mmean,mmed,mgmean)
mretcomp2

# 3.3. Returns Normality

# 3.3.1. Skewness
mskew <- skewness(mlstocks)
mskew

# 3.3.2. Kurtosis
mkurt <- kurtosis(mlstocks)
mkurt

# 3.3.3. Jarque-Bera Test
mjb <- jarque.bera.test(mlstocks)
mjb

# 3.3.4. Value at Risk
mvar <- VaR(mlstocks,p=0.99,method="modified")
colnames(mvar) <- c("mvar")
mvar

# 3.4. Returns and Risks Relationships

# 3.4.1. Covariance
mmain <- data.frame(mcash,mbonds,mstocks)
macov <- cov(mmain)
macov
mstocksc <- data.frame(mlstocks,mstocks)
mscov <- cov(mstocksc)
mscov

# 3.4.2. Correlation
macor <- cor(mmain)
macor
mscor <- cor(mstocksc)
mscor

# 3.4.3. Coefficient of Determination
mar2 <- macor ^ 2
mar2
msr2 <- mscor ^ 2
msr2

# 3.5. Assets and Market Relationship (Systematic Risk)

# 3.5.1. Assets Pricing Models Data
# CPI <- Quandl("FRED/CPIAUCSL", type="xts", start_date="2007-01-01", end_date="2017-01-01")
# cpi <- CPI
cpi <- data[,24]
mcpi <- monthlyReturn(cpi)
colnames(mcpi) <- "mcpi"
mmom <- data[,19]/100
msprem <- data[,20]/100
msize <- data[,21]/100
mstyle <- data[,22]/100
mrisk <- data[,23]/100
mlsprem <- mlstocks-mrisk

# 3.5.2. Capital Asset Pricing Model CAPM

# CAPM Regression
mregc <- lm(mlsprem~msprem)
summary(mregc)

# CAPM Beta Coefficient
mbcapm <- CAPM.beta(Ra=mlstocks,Rb=msprem+mrisk,Rf=mrisk)
mbcapm

# CAPM Jensen's Alpha Intercept
macapm <- CAPM.alpha(Ra=mlstocks,Rb=msprem+mrisk,Rf=mrisk)
macapm

# CAPM Expected Returns
mcapm <- mean(mrisk)+mbcapm*(mean(msprem))
mcapm

# CAPM Residual Variance (Unsystematic Risk)
mresvarc <- summary(mregc)$sigma
mresvarc

# 3.5.3. Fama-French-Carhart Factors Model

# Fama-French-Carhart Regression
mfactf <- cbind(msprem,msize,mstyle,mmom)
mregf <- lm(mlsprem~mfactf)
summary(mregf)

# Fama-French-Carhart Beta Coefficients
msprembf <- summary(mregf)$coefficients[2]
msizeb <- summary(mregf)$coefficients[3]
mstyleb <- summary(mregf)$coefficients[4]
mmomb <- summary(mregf)$coefficients[5]
mbffc <- cbind(msprembf,msizeb,mstyleb,mmomb)
mbffc

# Fama-French-Carhart Alpha Intercept
maffc <- summary(mregf)$coefficients[1]
maffc

# Fama-French-Carhart Expected Returns
mffc <- mean(mrisk)+maffc+msprembf*mean(msprem)+msizeb*mean(msize)+mstyleb*mean(mstyle)+mmomb*mean(mmom)
mffc

# Fama-French-Carhart Residual Variance (Unsystematic Risk)
mresvarf <- summary(mregf)$sigma
mresvarf

# 3.5.3. Arbitrage Pricing Theory Model APT

# Arbitrage Pricing Theory Regression
mfacta <- cbind(msprem,mcpi)
mrega <- lm(mlsprem~mfacta)
summary(mrega)

# Arbitrage Pricing Theory Coefficients
mspremba <- mrega$coefficients[2]
mcpib <- mrega$coefficients[3]
mbapt <- cbind(mspremba,mcpib)
mbapt

# Arbitrage Pricing Theory Alpha Intercept
maapt <- mrega$coefficients[1]
maapt

# Arbitrage Pricing Theory Expected Returns
mapt <- mean(mrisk)+maapt+mspremba*mean(msprem)+mcpib*mean(mcpi)
names(mapt) <- "mapt"
mapt

# Arbitrage Pricing Theory Residual Variance (Unsystematic Risk)
mresvara <- summary(mrega)$sigma
mresvara

# 3.5.4. Expected Returns Third Comparison
retcomp3 <- cbind(mmean,mmed,mgmean,mcapm,mffc,mapt)
retcomp3

# 3.5.5. Excess Returns Comparison
eretcomp <- cbind(macapm,maffc,maapt)
eretcomp

# 3.5.6. Residual Variance Comparison (Unsystematic Risk)
resvarcomp <- cbind(mresvarc,mresvarf,mresvara)
resvarcomp

# 3.6. Systematic Risk Hedge

# 3.6.1. Put Call Parity
# CBOE BXM Index
# getSymbols("^BXM",src="yahoo",from="2007-01-01",to="2017-01-01")
# parity <- BXM[,4]
parity <- data[,25]
mparity <- monthlyReturn(parity)
colnames(mparity) <- "mparity"
# Put Call Parity Comparison
mstocks2 <- msprem+mrisk
colnames(mstocks2) <- "mstocks2"
paritycomp <- cbind(mstocks2, mparity)
table.AnnualizedReturns(paritycomp)
charts.PerformanceSummary(paritycomp)

# 3.6.2. Tail Risk Hedge
# CBOE VXTH Index
# getSymbols("^VXTH",src="yahoo",from="2007-01-01",to="2017-01-01")
# tail <- VXTH[,4]
tail <- data[,26]
mtail <- monthlyReturn(tail)
colnames(mtail) <- "mtail"
# Tail Risk Comparison
tailcomp <- cbind(mstocks2, mtail)
table.AnnualizedReturns(tailcomp)
charts.PerformanceSummary(tailcomp)

# 3.7. Hedge Funds
# Eureka Equal Weighted Hedge Fund Index
# HFR <- Quandl("EUREKA/473", type="xts", start_date="2007-01-01", end_date="2017-01-01")
# hfr <- HFR
hfr <- data[,27]
mhedge <- hfr/100
colnames(mhedge) <- "mhedge"
# Hedge Funds Comparison
hedgecomp <- cbind(mlstocks, mhedge)
table.AnnualizedReturns(hedgecomp)
charts.PerformanceSummary(hedgecomp)

# 3.8. Portfolio Leverage

# 3.8.1. Maximum Leverage Calculation

# Maximum Annual Leverage Ratio
alstocks <- yearlyReturn(stocks)
acash <- yearlyReturn(cash)
amaxlev <- 1/abs(min(alstocks)-max(acash))
amaxlev

# 3.8.2. U.S. Large Cap Stock Market 2x Daily Leverage
# getSymbols("SSO",src="yahoo",from="2007-01-01",to="2017-01-01")
# lstocks2x <- SSO[,6]
lstocks2x <- data[,28]
mlstocks2x <- monthlyReturn(lstocks2x)
colnames(mlstocks2x) <- "mlstocks2x"

# 3.8.3. Leveraged Portfolios Returns Comparison
levcomp <- cbind(mlstocks, mlstocks2x)
table.AnnualizedReturns(levcomp)
charts.PerformanceSummary(levcomp)

##########################################

# 4. Portfolio Optimization

# 4.1. Portfolio Performance Metrics

# 4.1.1. Sharpe Ratio
msharpe <- SharpeRatio(mlstocks,Rf=mean(mcash))
msharpe

# 4.1.2. Treynor Ratio
mtreynor <- TreynorRatio(Ra=mlstocks,Rb=mstocks,Rf=mean(mcash))
mtreynor

# 4.1.3. Sortino Ratio
msortino <- SortinoRatio(R=mlstocks,MAR=mean(mcash))
msortino

# 4.1.4. Kelly Ratio
mkelly <- KellyRatio(R=mlstocks,Rf=mean(mcash),method="half")
mkelly

# 4.2. Portfolio Benchmarks

# 4.2.1. Portfolio Assets Returns Matrix
mport <- cbind(mbonds,mstocks,mibonds,mistocks)

# 4.2.2. Naive Global Portfolio (Monthly Rebalancing)
mnaivew <- as.numeric(t(c(0.25,0.25,0.25,0.25)))
names(mnaivew) <- c("mbonds","mstocks","mibonds","mistocks")
mnaive <- Return.portfolio(R=mport,weights=mnaivew,geometric=F,rebalance_on="months")
colnames(mnaive) <- "mnaive"

# 4.2.3. Roche Global Portfolio (Monthly Rebalancing)
mrochew <- as.numeric(t(c(0.24,0.18,0.33,0.25)))
names(mrochew) <- c("mbonds","mstocks","mibonds","mistocks")
mroche <- Return.portfolio(R=mport,weights=mrochew,geometric=F,rebalance_on="months")
colnames(mroche) <- "mroche"

# 4.2.4. Bogle U.S. Portfolio (Monthly Rebalancing)
mboglew <- as.numeric(t(c(0.40,0.60,0.00,0.00)))
names(mboglew) <- c("mbonds","mstocks","mibonds","mistocks")
mbogle <- Return.portfolio(R=mport,weights=mboglew,geometric=F,rebalance_on="months")
colnames(mbogle) <- "mbogle"

# 4.2.6. Benchmark Portfolios Returns Comparison
benchcomp <- cbind(mnaive, mroche, mbogle)
table.AnnualizedReturns(benchcomp)
charts.PerformanceSummary(benchcomp)

# 4.3. Portfolio Optimization

# 4.3.1. Mean Maximization Portfolio

# Portfolio Specifications
mport1c <- portfolio.spec(assets = colnames(mport))

# Portfolio Constraints
mport1c <- add.constraint(mport1c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport1c <- add.constraint(mport1c,type="long_only")

# Portfolio Objectives
mport1c <- add.objective(mport1c,type="return",name="mean")

# Portfolio Optimization
mportopt1 <- optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport1c,optimize_method="DEoptim",search_size=20000,trace=T)
chart.Weights(mportopt1)

# Portfolio Backtesting (Monthly Rebalancing)
mport1 <- Return.portfolio(R=mport["2015-01-31::"],weights=extractWeights(mportopt1),geometric=F,rebalance_on="months")
colnames(mport1) <- "mport1"
mportcomp1 <- cbind(mnaive["2015-01-31::"],mroche["2015-01-31::"],mbogle["2015-01-31::"],mport1)
table.AnnualizedReturns(mportcomp1)
charts.PerformanceSummary(mportcomp1)

# 4.3.2. Standard Deviation Minimization Portfolio

# Portfolio Specifications
mport2c <- portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport2c <- add.constraint(mport2c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport2c <- add.constraint(mport2c,type="long_only")

# Portfolio Objectives
mport2c <- add.objective(mport2c,type="risk",name="StdDev")

# Portfolio Optimization
mportopt2 <- optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport2c,optimize_method="DEoptim",search_size=20000,trace=T)
chart.Weights(mportopt2)

# Portfolio Backtesting (Monthly Rebalancing)
mport2 <- Return.portfolio(R=mport["2015-01-31::"],weights=extractWeights(mportopt2),geometric=F,rebalance_on="months")
colnames(mport2) <- "mport2"
mportcomp2 <- cbind(mnaive["2015-01-31::"],mroche["2015-01-31::"],mbogle["2015-01-31::"],mport2)
table.AnnualizedReturns(mportcomp2)
charts.PerformanceSummary(mportcomp2)

# 4.3.3. Mean Maximization and Standard Deviation Minimization Portfolio

# Portfolio Specifications
mport3c <- portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport3c <- add.constraint(mport3c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport3c <- add.constraint(mport3c,type="long_only")

# Portfolio Objectives
mport3c <- add.objective(mport3c,type="return",name="mean")
mport3c <- add.objective(mport3c,type="risk",name="StdDev")

# Portfolio Optimization
mportopt3 <- optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport3c,optimize_method="DEoptim",search_size=20000,trace=T)
chart.Weights(mportopt3)
chart.EfficientFrontier(mportopt3,match.col="StdDev")

# Portfolio Backtesting (Monthly Rebalancing)
mport3 <- Return.portfolio(R=mport["2015-01-31::"],weights=extractWeights(mportopt3),geometric=F,rebalance_on="months")
colnames(mport3) <- "mport3"
mportcomp3 <- cbind(mnaive["2015-01-31::"],mroche["2015-01-31::"],mbogle["2015-01-31::"],mport3)
table.AnnualizedReturns(mportcomp3)
charts.PerformanceSummary(mportcomp3)

# 4.3.4. Mean Maximization Value at Risk (VaR) Minimization Portfolio

# Portfolio Specifications
mport4c <- portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport4c <- add.constraint(mport4c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport4c <- add.constraint(mport4c,type="long_only")

# Portfolio Objectives
mport4c <- add.objective(mport4c,type="return",name="mean")
mport4c <- add.objective(mport4c,type="risk",name="VaR",arguments=list(p = 0.99,method="modified"))

# Portfolio Optimization
mportopt4 <- optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport4c,optimize_method="DEoptim",search_size=20000,trace=T)
chart.Weights(mportopt4)

# Portfolio Backtesting (Monthly Rebalancing)
mport4 <- Return.portfolio(R=mport["2015-01-31::"],weights=extractWeights(mportopt4),geometric=F,rebalance_on="months")
colnames(mport4) <- "mport4"
mportcomp4 <- cbind(mnaive["2015-01-31::"],mroche["2015-01-31::"],mbogle["2015-01-31::"],mport4)
table.AnnualizedReturns(mportcomp4)
charts.PerformanceSummary(mportcomp4)

# 4.3.5. Optimized Portfolios Backtesting Comparison
mportcomp = cbind(mnaive["2015-01-31::"],mport1,mport2,mport3,mport4)
table.AnnualizedReturns(mportcomp)
charts.PerformanceSummary(mportcomp)

##########################################

# 5. Portfolio Performance

# 5.1. Portfolio Performance Data
misprem <- data[,29]/100
mterm <- mlbonds-mrisk
mfactp <- cbind(msprem,misprem,mterm)
colnames(mfactp) <- c("msprem","misprem","mterm")
mfactp <- mfactp["2015-01-31::"]

# 5.2. Mean Maximization Portfolio Performance

# 5.2.1. Mean Maximization Portfolio Factors Regressions
extractWeights(mportopt1)
mpprem1 <- mport1-mrisk["2015-01-31::"]
mregp1 <- lm(mpprem1~mfactp)
summary(mregp1)

mfactp1 <- cbind(mfactp[,1],mfactp[,3])
colnames(mfactp1) <- c("msprem","mterm")
mregp1f <- lm(mpprem1~mfactp1)
summary(mregp1f)

# 5.2.2. Mean Maximization Portfolio Expected Returns
map1 <- summary(mregp1f)$coefficients[1]
mspreme1 <- summary(mregp1f)$coefficients[2]*mean(mfactp1[,1])
mterme1 <- summary(mregp1f)$coefficients[3]*mean(mfactp1[,2])
mppreme1 <- map1+mspreme1+mterme1

mportcompe1 <- cbind(mppreme1,map1,mspreme1,mterme1)
colnames(mportcompe1) <- c("mppreme1","map1","mspreme1","mterme1")

mportcompb1 <- mportcompe1*100
bp1 <- barplot(mportcompb1,main="Mean Maximization Portfolio Expected Returns",col="lightgray",ylim=c(-0.1,0.7))
text(bp1,mportcompb1,labels=round(mportcompb1,2),pos=3)

# 5.3. Standard Deviation Minimization Portfolio Performance

# 5.3.1. Standard Deviation Minimization Portfolio Factors Regression
extractWeights(mportopt2)
mpprem2 <- mport2-mrisk["2015-01-31::"]
mregp2 <- lm(mpprem2~mfactp)
summary(mregp2)

mfactp2 <- cbind(mfactp[,2],mfactp[,3])
colnames(mfactp2) <- c("misprem","mterm")
mregp2f <- lm(mpprem2~mfactp2)
summary(mregp2f)

# 5.3.2. Standard Deviation Minimization Portfolio Expected Returns
map2 <- summary(mregp2f)$coefficients[1]
mispreme2 <- summary(mregp2f)$coefficients[2]*mean(mfactp2[,1])
mterme2 <- summary(mregp2f)$coefficients[3]*mean(mfactp2[,2])
mppreme2 <- map2+mispreme2+mterme2

mportcompe2 <- cbind(mppreme2,map2,mispreme2,mterme2)
colnames(mportcompe2) <- c("mppreme2","map2","mispreme2","mterme2")

mportcompb2 <- mportcompe2*100
bp2 <- barplot(mportcompb2,main="Standard Deviation Minimization Portfolio Expected Returns",col="lightgray",ylim=c(-0.05,0.15))
text(bp2,mportcompb2,labels=round(mportcompb2,2),pos=3)

# 5.4. Mean Maximization and Standard Deviation Minimization Portfolio Performance

# 5.4.1. Mean Maximization and Standard Deviation Minimization Portfolio Factors Regression
extractWeights(mportopt3)
mpprem3 <- mport3-mrisk["2015-01-31::"]
mregp3 <- lm(mpprem3~mfactp)
summary(mregp3)

mfactp3 <- cbind(mfactp[,2],mfactp[,3])
colnames(mfactp3) <- c("misprem","mterm")
mregp3f <- lm(mpprem3~mfactp3)
summary(mregp3f)

# 5.4.2. Mean Maximization and Standard Deviation Minimization Portfolio Expected Returns
map3 <- summary(mregp3f)$coefficients[1]
mispreme3 <- summary(mregp3f)$coefficients[2]*mean(mfactp3[,1])
mterme3 <- summary(mregp3f)$coefficients[3]*mean(mfactp3[,2])
mppreme3 <- map3+mispreme3+mterme3

mportcompe3 <- cbind(mppreme3,map3,mispreme3,mterme3)
colnames(mportcompe3) <- c("mppreme3","map3","mispreme3","mterme3")

mportcompb3 <- mportcompe3*100
bp3 <- barplot(mportcompb3,main="Mean Maximization and Standard Deviation Minimization Portfolio Expected Returns",col="lightgray",ylim=c(-0.05,0.15))
text(bp3,mportcompb3,labels=round(mportcompb3,2),pos=3)

# 5.5. Mean Maximization and VaR Minimization Portfolio Performance

# 5.5.1. Mean Maximization and VaR Minimization Portfolio Factors Regression
extractWeights(mportopt4)
mpprem4 <- mport4-mrisk["2015-01-31::"]
mregp4 <- lm(mpprem4~mfactp)
summary(mregp4)

mfactp4 <- cbind(mfactp[,2],mfactp[,3])
colnames(mfactp4) <- c("misprem","mterm")
mregp4f <- lm(mpprem4~mfactp4)
summary(mregp4f)

# 5.5.2. Mean Maximization and VaR Minimization Portfolio Expected Returns
map4 <- summary(mregp4f)$coefficients[1]
mispreme4 <- summary(mregp4f)$coefficients[2]*mean(mfactp4[,1])
mterme4 <- summary(mregp4f)$coefficients[3]*mean(mfactp4[,2])
mppreme4 <- map4+mispreme4+mterme4

mportcompe4 <- cbind(mppreme4,map4,mispreme4,mterme4)
colnames(mportcompe4) <- c("mppreme4","map4","mispreme4","mterme4")

mportcompb4 <- mportcompe4*100
bp4 <- barplot(mportcompb4,main="Mean Maximization and VaR Minimization Portfolio Expected Returns",col="lightgray",ylim=c(-0.05,0.15))
text(bp4,mportcompb4,labels=round(mportcompb4,2),pos=3)

# 5.6. Investment Costs
astocks2 <- Return.annualized(mstocks2)
astocks2l <- astocks2-0.0005
astocks2h <- astocks2-0.0075
costscomp1 <- cbind(astocks2,astocks2l,astocks2h)
costscomp1
cstocks2 <- (1+astocks2)^10-1
cstocks2l <- (1+astocks2l)^10-1
cstocks2h <- (1+astocks2h)^10-1
costscomp2 <- cbind(cstocks2,cstocks2l,cstocks2h)
costscomp2
