#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("plyr")

library(reshape2)
library(ggplot2)
library(plyr)

#STEP 0: Load sp500_options_2010 data, calculate mid price of options

sp500_options_2010=read.csv("C:/Users/Pruthvi/Desktop/Complex Financial Instruments/Case Study/sp500_options_2010.csv", sep=",",header=T)
sp500_options_2010[,"MID"]=(sp500_options_2010[,"BID_PRICE"] + sp500_options_2010[,"ASK_PRICE"])/2
sp500_options_2010[,"STRIKEPRICE"]=sp500_options_2010[,"STRIKEPRICE"]/1000
drop=c("UNDERLYING", "IMPL_VOLATILITY", "DELTA", "GAMMA", "VEGA", "THETA")
sp500_options_2010=sp500_options_2010[,!(names(sp500_options_2010) %in% drop)]
in_r=0
in_date_start="2010-01-04"
# in_date_start="2010-02-05"
in_MATURITYDATE=20100220
head(sp500_options_2010)

#STEP 1: Calculate days to expiry

sp500_options_2010[,"MATURITYDATE"]=as.numeric(sp500_options_2010[,"MATURITYDATE"])
sp500_options_2010[,"date_start"]=as.Date(sp500_options_2010[,"Time"], format="%e-%b-%y")
sp500_options_2010[,"date_end"]=as.Date(as.character(sp500_options_2010[,"MATURITYDATE"]), format="%Y%m%d")
sp500_options_2010[,"days_to_expiry"]=sp500_options_2010[,"date_end"] - sp500_options_2010[,"date_start"]
sp500_options_2010[,"days_to_expiry"]=as.numeric(sp500_options_2010[,"days_to_expiry"])

theme_report=function(base_size=22, base_family="",...)
{
  theme_bw(base_size= base_size, base_family= base_family)+
    theme(
      legend.position="top",
      legend.justification='left',
      #Legend.position=c(.88,.68),
      line=element_line(colour="black"),
      axis.text= element_text(angle=0),
      legend.text = element_text(angle=0,size=base_size * 1.0),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(angle=0,size=base_size* 1.0),
      legend.key.width=unit(3, "cm"),
      panel.grid.minor= element_blank(),
      panel.grid.major= element_blank(),
      panel.border= element_rect(fill=NA, colour = "black")
    )
}

data=subset(sp500_options_2010, MATURITYDATE==in_MATURITYDATE & date_start==as.Date(in_date_start))
ggplot(data,aes(x=STRIKEPRICE, y=BID_PRICE)) + geom_line(aes(color=OPTIONTYPE)) + theme_report()

#Step 2: Finding forward price and K0

# cut all data required to find ATM strikes:
atm_strikes=sp500_options_2010[,c("date_start", "MATURITYDATE", "days_to_expiry", "OPTIONTYPE", "STRIKEPRICE", "MID")]

# convert atm_strikes from long format to wide format
atm_strikes=dcast(atm_strikes, formula = date_start + MATURITYDATE + days_to_expiry + STRIKEPRICE ~ OPTIONTYPE, value.var="MID")
atm_strikes[,"CmP"]=atm_strikes[,"C"] - atm_strikes[,"P"]

get_forwards=function (options, interest_rate)
{
  forwards=ddply(options, .(date_start, MATURITYDATE), function(x) {
    x[which.min(abs(x[, "CmP"])), ]
  })
  forwards[, "Forward"] = forwards[, "STRIKEPRICE"] + exp(interest_rate *
                                                            forwards[, "days_to_expiry"]/365) * forwards[, "CmP"]
  forwards= forwards[, c("date_start", "MATURITYDATE", "Forward")]
  options= merge(options, forwards[, c("date_start", "MATURITYDATE",
                                       "Forward")], by = c("date_start", "MATURITYDATE"))
  return(options)
}

forwards= get_forwards(atm_strikes, in_r)
head(forwards[,-(5:6)])

get_kzero=function (options)
{
  atm_strikes= subset(options, Forward >= STRIKEPRICE)
  atm_strikes[, "KmF"]=round((atm_strikes[, "Forward"] - atm_strikes[, "STRIKEPRICE"]), 2)
  kzero= ddply(atm_strikes, .(date_start, MATURITYDATE),
               function(x) {
                 x[which.min(x[, "KmF"]),]
               })
  kzero[, "KZERO"]=kzero[, "STRIKEPRICE"]
  return(kzero)
}

kzero= get_kzero(forwards)
head(kzero[,-(5:7)])

sp500_options_2010= merge(sp500_options_2010, kzero[,c("date_start","MATURITYDATE", "Forward",
                                                       "KZERO")], by=c("date_start", "MATURITYDATE"))
head(sp500_options_2010)

#1 Example option selection: with indication of where to take Call and Put options

data= subset(sp500_options_2010, MATURITYDATE==in_MATURITYDATE & date_start==as.Date(in_date_start))
kzero=data[1,"KZERO"]

ggplot(data,aes(x=STRIKEPRICE, y=BID_PRICE)) +
  geom_line(aes(color=OPTIONTYPE)) +
  geom_point(aes(color=OPTIONTYPE)) +
  geom_vline(aes(xintercept = kzero)) +
  annotate("text", x=kzero-100, y=300, label= "P") +
  annotate("text", x=kzero+100, y=300, label= "C") + theme_report()

#2 Example option selection: only using OTM options

vix_options= subset(sp500_options_2010, (sp500_options_2010[,"OPTIONTYPE"] == "C" & STRIKEPRICE > KZERO) | (sp500_options_2010[,"OPTIONTYPE"] == "P" & STRIKEPRICE < KZERO))
data= subset(vix_options, MATURITYDATE==in_MATURITYDATE & date_start==as.Date(in_date_start))
kzero= data[1,"KZERO"]
ggplot(data,aes(x=STRIKEPRICE, y=BID_PRICE)) +
  geom_line(aes(color=OPTIONTYPE)) +
  geom_point(aes(color=OPTIONTYPE)) +
  geom_vline(aes(xintercept = kzero)) +
  annotate("text", x=kzero-100, y=300, label= "P") +
  annotate("text", x=kzero+100, y=300, label= "C") + ylim(0,1)+theme_report()

#Step 3: Finding Double Zero Bid Prices

.add_lagged_column=function (data, column_name, orderby)
{
  data = ddply(data, .(Time, days_to_expiry), function(x) {
    x = x[order(x[, orderby]),]
    n = length(row.names(x))
    x[,paste("LAST_", column_name, sep = "")]=c(NA, x[1:(n-1), column_name])
    return(x)
  })
  return(data)
}

get_doublezerobids=function(options)
{
  options=.add_lagged_column(options,column_name = "BID_PRICE", orderby="STRIKEPRICE")
  zero_bids = options[, c("Time", "OPTIONTYPE", "days_to_expiry","STRIKEPRICE", "BID_PRICE", "LAST_BID_PRICE")]
  zero_bids = subset(zero_bids, BID_PRICE == 0 & LAST_BID_PRICE ==0)
  zero_bids = ddply(zero_bids, .(Time, OPTIONTYPE, days_to_expiry),summarize, min_k = min(STRIKEPRICE), max_k = max(STRIKEPRICE))
  return(zero_bids)
}

doublezerobids = get_doublezerobids(vix_options)
vix_options = merge(vix_options, doublezerobids, by = c("Time", "OPTIONTYPE", "days_to_expiry"), all.x=T)

#3 Example option selectionS: indicating minimum and maximum strike prices

data = subset(vix_options, MATURITYDATE==in_MATURITYDATE & date_start==as.Date(in_date_start))
calls = subset(data, OPTIONTYPE=="C")
puts = subset(data, OPTIONTYPE=="P")
kzero = data[1,"KZERO"]
calls = subset(data, OPTIONTYPE=="C")
kmin = calls[1,"min_k"]
puts = subset(data, OPTIONTYPE=="P")
kmax = puts[1,"max_k"]

ggplot(data,aes(x=STRIKEPRICE, y=BID_PRICE)) +
  geom_line(aes(color=OPTIONTYPE)) +
  geom_point(aes(color=OPTIONTYPE)) +
  geom_vline(xintercept = kzero) +
  geom_vline(xintercept = kmax) +
  geom_vline(xintercept = kmin) +
  annotate("text", x=kzero-100, y=300, label= "P") +
  annotate("text", x=kzero+100, y=300, label= "C") +
  ylim(0, 30) +
  theme_report()

head(vix_options)

vix_atm_options = subset(sp500_options_2010, STRIKEPRICE == KZERO)
vix_options = vix_options[,1:12]
vix_options = rbind(vix_atm_options, vix_options)
vix_options = vix_options[order(vix_options[,"date_start"],vix_options[,"MATURITYDATE"],vix_options[,"STRIKEPRICE"]),]

get_previousstrike=function (options)
{
  options = .add_lagged_column(options, column_name = "STRIKEPRICE",
                               orderby = "STRIKEPRICE")
  return(options)
}

vix_options = get_previousstrike(vix_options)
variance_swap = ddply(vix_options, .(date_start,MATURITYDATE,days_to_expiry), function(x)
{
  T = x[1,"days_to_expiry"]/365
  F = x[1,"Forward"]
  K = x[1,"KZERO"]
  # Note, need to take mid-point between two strikes, i.e., divided by 2
  # Also need to check ATM strike, because currently just takes the previous one, which is also an ATM strike, hence the difference would be 0!
  
  x[,"Delta"]= (x[,"STRIKEPRICE"]-x[,"LAST_STRIKEPRICE"])/(x[,"STRIKEPRICE"]^2)
  
  # Check what to do with the first strike in the sum
  
  sum_otm= sum(x[,"Delta"]*x[,"MID"],na.rm=T)
  price= 2/T * exp(in_r*T) * sum_otm - 1/T*(F/K -1)^2
  return(price)
})

vix_prices= variance_swap
vix_prices[,"V1"]= sqrt(vix_prices[,"V1"])
head(vix_prices)

ggplot(subset(vix_prices,days_to_expiry < 20),aes(x=date_start,y=V1)) + geom_line(aes(color=as.factor(days_to_expiry) )) + theme_report()

#Step 4: Calculating 30-Day Volatility Index

vix_prices$diff= abs(vix_prices$days_to_expiry-30)
vix1= vix_prices[order(vix_prices$date_start, vix_prices$diff),]
vix2= split(vix1,vix1$date_start)
r1=lapply(vix2,FUN=function(vix2) T=vix2[c(1),])
r11=ldply(r1,data.frame)
r2=lapply(vix2,FUN=function(vix2) T=vix2[c(2),])
r22=ldply(r2, data.frame)
r22$v2=r22$V1
r22$days_to_expiry2=r22$days_to_expiry
r4=data.frame(r11$date_start,r11$days_to_expiry,r11$V1,r22$days_to_expiry2,r22$v2)
head((r4))

T1=r4[,"r11.days_to_expiry"]/365
T2=r4[,"r22.days_to_expiry2"]/365
Sigma1=r4[,"r11.V1"]
Sigma2=r4[,"r22.v2"]
a= (T1 * (Sigma1 ^ 2) * ((T2-30/365)/(T2-T1)))
b= (T2 * (Sigma2 ^ 2) * ((30/365)-T1/(T2-T1)))
VIX30=  100*sqrt((a+b)*(365/30))
head(VIX30)


#Step 5: Calculating correlation between 30-day volatility index & actual VIX closing figure

vix_yahoo= read.csv("C:/Users/Pruthvi/Desktop/Complex Financial Instruments/Case Study/VIX.csv", sep=",",header=T)
head(vix_yahoo)

vix_yahoo_close= vix_yahoo[,"Adj.Close"]
cor(VIX30,vix_yahoo_close)

#Step 6: Calculating 93-day volatility Index

vix_prices$diff= abs(vix_prices$days_to_expiry-93)
vix3= vix_prices[order(vix_prices$date_start,vix_prices$diff),]
vix4= split(vix3,vix3$date_start)
r11= lapply(vix4, FUN=function(vix4) T=vix4[c(1),])
r111= ldply(r11,data.frame)
r22=lapply(vix4, FUN=function(vix4) T=vix4[c(2),] )
r222= ldply(r22, data.frame)
r222$v2= r222$V1
r222$days_to_expiry2= r222$days_to_expiry
r44= data.frame(r111$date_start,r111$days_to_expiry, r111$V1, r222$days_to_expiry2,r222$v2)
head(r44)

T1_1=r44[,"r111.days_to_expiry"]/365
T2_2=r44[,"r222.days_to_expiry2"]/365
Sigma1=r44[,"r111.V1"]
Sigma2=r44[,"r22.v2"]
c= (T1_1 * (Sigma1 ^ 2) * ((T2_2-93/365)/(T2_2-T1_1)))
d= (T2_2 * (Sigma2 ^ 2) * ((93/365)-T1_1/(T2_2-T1_1)))
VIX93= 100 * sqrt((c+d) * (365/93))
head(VIX93)

#Step 7: Calculating correlation betweeb 93-day volatility & actual VXV closing figure

vxv_yahoo= read.csv("C:/Users/Pruthvi/Desktop/Complex Financial Instruments/Case Study/VXV.csv", sep=",",header=T)
head(vxv_yahoo)

vxv_yahoo_close= vxv_yahoo[,"Adj.Close"]
cor(VIX93, vxv_yahoo_close)

#Step 8: Calculating correlation between 30-day volatility index & actual SPY closing price

spy_yahoo= read.csv("C:/Users/Pruthvi/Desktop/Complex Financial Instruments/Case Study/SPY.csv", sep=",",header=T)
spy_yahoo_close= spy_yahoo[,"Adj.Close"]
n=nrow(spy_yahoo)
e=as.numeric(matrix(spy_yahoo$Adj.Close))
ret= NA
for (i in 1:(n-1)){
  ret[i]=log(e[i+1]/e[i])
}
head(ret)

m=length(VIX30)
f= as.numeric(VIX30)
ret1=NA
for(i in 1:(m-1)){
  ret1[i]=log(f[i+1]/f[i])
}
cor(ret,ret1)

cor(spy_yahoo_close,vix_yahoo_close)


