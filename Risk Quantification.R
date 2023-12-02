#Import the database of 10 assets from Excel into R
install.packages("xts")
install.packages("readxl")
library("readxl")
library(xts)
library(readxl)
our_portfolio <- read_excel("~/Dropbox/Risk Management/Final_portfolio_data (updated Amazon).xlsx", 
                            col_types = c("date", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"))
View(our_portfolio)

# Convert our_portfolio$Date to #Date
our_portfolio$Date <-as.Date(our_portfolio$Date, "%Y-%m-%d")
class(our_portfolio$Date)

# Convert our_portfolio dataframe into xts object
our_portfolio<-as.xts(our_portfolio[,-1], our_portfolio$Date)
class(our_portfolio)
end(our_portfolio)

#Omit missing data
our_portfolio<-na.omit(our_portfolio)
our_portfolio
class(our_portfolio)

#Check if all missing data has been deleted
sum(is.na(our_portfolio))

#Plot our_portfolio
plot.zoo(our_portfolio,main="Closing Prices of 10 Assets", xlab= "Date",col="black")

#============================================

#POINT 3

#calculate logarithmic daily returns
log_returns <- diff(log(our_portfolio), lag=1)
log_returns

#count missing values
sum(is.na(log_returns))

#omit missing data again
log_returns<-na.omit(log_returns)

#check whether there's still missing data
sum(is.na(log_returns)) 

#see log daily returns
log_returns

#plot time series
plot(log_returns$Sugar)
plot(log_returns$BTC)
plot(log_returns$Natural)
plot(log_returns$Bayer)
plot(log_returns$USD)
plot(log_returns$YUM)
plot(log_returns$Disney)
plot(log_returns$Tiffany)
plot(log_returns$Wirecard)
plot(log_returns$Amazon)

#plot histograms
hist(log_returns$Sugar,nclass = 50)
hist(log_returns$BTC, nclass = 50)
hist(log_returns$Natural, nclass = 100)
hist(log_returns$Bayer)
hist(log_returns$USD)
hist(log_returns$YUM)
hist(log_returns$Disney)
hist(log_returns$Tiffany)
hist(log_returns$Wirecard) 
hist(log_returns$Amazon)

#install packages to create all time series in one
install.packages("maptools")
library("maptools")

#alternative way for plotting all time series in one
plot.zoo(log_returns, main="Portfolio - Daily Logarithmic Returns from 2016 to 2020", xlab="Date")

#alternative way for merging all histograms in one
install.packages("HistogramTools")
library("HistogramTools")
hist1 <- hist(log_returns$Sugar, breaks=-5:5, plot=FALSE)
hist2 <-hist(log_returns$BTC, breaks=-5:5, plot=FALSE)
hist3 <-hist(log_returns$Natural, breaks=-5:5, plot=FALSE)
hist4 <-hist(log_returns$Bayer, breaks=-5:5, plot=FALSE)
hist5 <-hist(log_returns$USD, breaks=-5:5, plot=FALSE)
hist6 <-hist(log_returns$YUM, breaks=-5:5, plot=FALSE)
hist7 <-hist(log_returns$Disney, breaks=-5:5, plot=FALSE)
hist8 <-hist(log_returns$Tiffany, breaks=-5:5, plot=FALSE)
hist9 <-hist(log_returns$Wirecard, breaks=-5:5, plot=FALSE) 
hist10 <-hist(log_returns$Amazon, breaks=-5:5, plot=FALSE)
hist.sum <- AddHistograms(hist1, hist2, hist3, hist4, hist5, hist6, hist7, hist8, hist9, hist10)

#============================================

#POINT 4

#create a Variance-Covariance Matrix
cov_matrix<-cov(log_returns)
cov_matrix

#test if matrix is positive semidefinite
install.packages("matrixcalc")
library("matrixcalc")
is.positive.semi.definite(cov_matrix)

#============================================

#POINT 5

#install package to calculate daily logarithmic returns 
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

#calculate daily logarithmic returns for the given assets, plot them and check the last lines 
portfolio_A_return <- Return.portfolio(log_returns, weights=NULL, value=1000000, verbose = FALSE)
plot.zoo(portfolio_A_return, main = "Log Portfolio A Returns")
print(portfolio_A_return)
tail(portfolio_A_return)

#ALTERNATIVE to first approach:
  
# Point 5 - Investment in naive portfolio
# Extract prices from oct 2 2020
price_021020<-our_portfolio[nrow(our_portfolio),]
price_021020

#Calculate amount of positions worth $100.000 on 02 Oct 2020
naive_positions<-100000/price_021020
naive_positions

#make naive positions valid for all days
?matrix
class(naive_portfolio)
naive_positions<-as.numeric(naive_positions)
naive_positions
matrix_naive_positions<-matrix(rep(naive_positions, times = NROW(our_portfolio)), byrow = TRUE, ncol = 10)
matrix_naive_positions

#Calculate Naive Portfolio
naive_portfolio<-our_portfolio*matrix_naive_positions


naive_portfolio$sum <- sum(naive_portfolio)
naive_portfolio

#'Check
naive_portfolio_021020<-naive_portfolio[nrow(naive_portfolio),]
naive_portfolio_021020

#Calculate daily log-returns
naive_portfolio_daily_return<-diff(log(naive_portfolio),lag=1)
naive_portfolio_daily_return

mean(naive_portfolio_daily_return)


#============================================

#POINT 6

#install package and create a mean-variance efficient portfolio 
install.packages ("tseries")
library(tseries)
portfolio_B <- portfolio.optim(log_returns, riskless=FALSE, shorts=FALSE, covmat = cov_matrix)
class(portfolio_B)

#create a vector of weights from optimized portfolio
portfolio_B_weights<- portfolio_B$pw
plot(portfolio_B_weights)

#assign asset names for portfolio weights
names(portfolio_B_weights)<-colnames(log_returns)

#visualize the distribution of optimized portfolio
barplot(portfolio_B_weights)

#portfolio B return with given weights
portfolio_B_return <- Return.portfolio(log_returns,weights = portfolio_B_weights,verbose = FALSE)
portfolio_B_return
plot.zoo(portfolio_B_return, main = "Portfolio B Returns")

#============================================

#POINT 7

#calculate means
mean_A <- mean(portfolio_A_return)
mean_B <- mean(portfolio_B_return)

#calculate variance
variance_A <- var(portfolio_A_return)
variance_B <- var(portfolio_B_return)

#calculate standard deviation
standarddev_A <- sqrt(variance_A)
standarddev_B <- sqrt(variance_B)

#95% VaR
VaR_A95 <- mean_A - 1.645 * standarddev_A 
VaR_B95 <- mean_B - 1.645 * standarddev_B 

#99% VaR
VaR_A99 <- mean_A - 2.33 * standarddev_A 
VaR_B99 <- mean_B - 2.33 * standarddev_B 

#10-day VaRs
VaR_A95_10days <- VaR_A95 * sqrt(10)
VaR_B95_10days <- VaR_B95 * sqrt(10)
VaR_A99_10days <- VaR_A99 * sqrt(10)
VaR_B99_10days <- VaR_B99 * sqrt(10)

#show 10-day VaRs
VaR_A95_10days
VaR_B95_10days
VaR_A99_10days
VaR_B99_10days
