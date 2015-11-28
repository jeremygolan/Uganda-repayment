rm(list = ls())

# uganda repayment prep
# written by: jeremy golan (jeremy.golan@oneacrefund.org)
# written for: Uganda Country Director
# last edited: 4 november 2015 (jg)

# directories
setwd("~/drive/nce_repayment")
wd <- ("~/drive/nce_repayment")
dd <- "~/drive/nce_repayment/data"
od <- "~/drive/nce_repayment/output"

#libraries
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)

# data input
# gather files to read in
files <- list.files(dd, pattern = ".csv")

# read in each dataset and put into variable
for(i in 1:length(files)) {
	tmp <- read.csv(paste(dd, files[i], sep = "/"), header = TRUE, 
  		sep = ",", stringsAsFactors = FALSE)

	# now assign data that reads in to an r object 
	assign(substr(files[i], 1, (nchar(files[i]) - 4)), tmp)
}


#lets input 2014 data
u1 <- uganda_VR
usc14 <- uganda_2014LR

#get data into right format
u1$date <- substr(u1$RepaymentDate, 1, 9)
u1$date <- gsub(" ", "", u1$date)
u1$date <- gsub("11/24/201", "11/24/2014", u1$date)
u1$date <- gsub("12/15/201", "12/15/2014", u1$date)
u1$date <- gsub("12/16/201", "12/16/2014", u1$date)
u1$date <- gsub("12/22/201", "12/22/2014", u1$date)


u1$date <- as.Date(u1$date, "%m/%d/%Y")


u2 <- subset(u1, date < as.Date("2014-9-1"))

#drop void receipts 

u3 <- subset(u2, !Type == "Void" )


#lets focus on 2015
u10 <- subset(u1, date  > as.Date("2014-11-1"))

u11 <- subset(u10, Type == "Receipt" )

u12 <- subset(u11, date < as.Date("2015-9-1"))

d <- subset(uganda_dropped2015, select = c(GlobalClientID,TotalCredit, Facilitator))
d$dropped2 <- 1
c <- subset(uganda_2015LR, select = c(GlobalClientID,TotalCredit, Facilitator))
c$dropped2 <- 0
cd <- rbind(c, d)

u13 <- merge(u12, cd, by = "GlobalClientID", all.y = T)

#we are going to assume global client iD is source of truth for 2015
test <- u13[which(is.na(u13$TotalCredit)) ,]
tail(test)
head(test)
length(unique(test$GlobalClientID))

table(u13$date[u13$dropped2 == 1], useNA = "ifany")

#lets drop NAs, but make sure to include the no payments
u13$no.payment <- u13$date
u13$no.payment <- ifelse(u13$dropped2 == 1 & is.na(u13$date), 1, u13$no.payment)
u13 <- u13[!is.na(u13$no.payment) ,]
u13$no.payment <- u13$date
u13$no.payment <- ifelse(u13$dropped2 == 1 & is.na(u13$date), 1, 0)

u13$month <- month(u13$date)

u13$Facilitator <- as.numeric(ifelse(u13$Facilitator == "True", 1, 0))


#lets vectorize
   #   test2 <- split(u13, u13$GlobalClientID)
#    x <- test2[[1026]]
re15 <- do.call(rbind, lapply(split(u13, u13$GlobalClientID),
  function(x) {
  
#percent repaid for april, may, june  
    april <- x
    
    april$value <- ifelse(april$date < as.Date("2015-05-01"), april$Amount, 0)
    
    april$sum <- ifelse(april$date >= as.Date("2015-05-01"), 0, sum(april$value))
 april$pct.repaid <- ifelse(april$sum == 0| april$TotalCredit == 0, 0, april$sum/april$TotalCredit)
 april$pct.repaid <- max(april$pct.repaid)
    
    may <- x
    
    may$value <- ifelse(may$date < as.Date("2015-06-01"), may$Amount, 0)
    may$sum <- ifelse(may$date >= as.Date("2015-06-01"), 0, sum(may$value, na.rm = T))
 may$pct.repaid <- ifelse(may$sum == 0| may$TotalCredit == 0, 0, may$sum/may$TotalCredit)
 may$pct.repaid <- max(may$pct.repaid)
    
    june <- x
    
    june$value <- ifelse(june$date < as.Date("2015-07-01"), june$Amount, 0)
 
    june$sum <- ifelse(june$date >= as.Date("2015-07-01"), 0, sum(june$value, na.rm = T))
 june$pct.repaid <- ifelse(june$sum == 0| june$TotalCredit == 0, 0, june$sum/june$TotalCredit)
 june$pct.repaid <- max(june$pct.repaid)

  #percent at prepayment
 
 #the soft deadline
 pp <- x
 
 pp$value <- ifelse(pp$date < as.Date("2015-02-09"), pp$Amount, 0)

    pp$sum <- ifelse(pp$date >= as.Date("2015-02-09"), 0, sum(pp$value))
 pp$pct.repaid <- ifelse(pp$sum == 0| pp$TotalCredit == 0, 0, pp$sum/pp$TotalCredit)
 pp$pct.repaid <- max(pp$pct.repaid)
 
   #percent at prepayment for that feb 15
 
 #some extended repayment 
  pp2 <- x
 
 pp2$value <- ifelse(pp2$date < as.Date("2015-02-16"), pp2$Amount, 0)

    pp2$sum <- ifelse(pp2$date >= as.Date("2015-02-16"), 0, sum(pp2$value))
 pp2$pct.repaid <- ifelse(pp2$sum == 0| pp2$TotalCredit == 0, 0, pp2$sum/pp2$TotalCredit)
 pp2$pct.repaid <- max(pp2$pct.repaid)

  pp3 <- x
 
 pp3$value <- ifelse(pp3$date < as.Date("2015-02-25"), pp3$Amount, 0)

    pp3$sum <- ifelse(pp3$date >= as.Date("2015-02-25"), 0, sum(pp3$value))
 pp3$pct.repaid <- ifelse(pp3$sum == 0| pp3$TotalCredit == 0, 0, pp3$sum/pp3$TotalCredit)
 pp3$pct.repaid <- max(pp3$pct.repaid)
 
#average size of payment in prepayment
 #using whole prepayment period

  ps <- x
 ps$pre <- ifelse(ps$date >= as.Date("2014-11-17") & ps$date <= as.Date("2015-02-16"), ps$Amount, 0)
  ps$pre[ps$pre == 0] <- NA
  ps$average <- mean(ps$pre, na.rm = T)
  
 #number of payments
ps$num <- length(which(!is.na(ps$pre)))
#0 repayments is same NA for average prepay  

 #date of first payment
ps$payday <- (as.Date(ifelse(ps$pre > 0, as.Date(ps$date), as.Date("2016-01-01"))))
 ps$first.date <- ifelse(ps$average > 0, min(ps$payday, na.rm = T), NA)
ps$first.date <- as.Date(ps$first.date)
ps$week <- as.numeric( format(ps$first.date+3, "%U"))
ps$first <- ifelse(ps$week < 40, ps$week + 52, ps$week)

 #time between payments
time <- data.frame(unique(ps$payday))
names(time)[1] <- "date"
time <- time[order(as.Date(time$date, format="%d/%m/%Y")),]
time <- na.omit(time)

ps$avg.days.btwn.pay <- mean(diff(time))

#people paying more than once on same date are creating NaN
#lets remove
ps$avg.days.btwn.pay <-ifelse(is.nan(ps$avg.days.btwn.pay) == T, NA, 
ps$avg.days.btwn.pay)

 #week they reached prepayment
 time2 <- subset(ps, select = (c(date,Amount, TotalCredit)))
time2 <- time2[order(as.Date(time2$date, format="%d/%m/%Y")),]
time2$week <- as.numeric( format(time2$date+3, "%U"))
time2$week <- ifelse(time2$week < 40, time2$week + 52, time2$week)
time2$cum <- cumsum(time2$Amount)
time2$cum.pct.repaid <- (time2$cum/time2$TotalCredit)
ps$reached <- min(time2$week[which(time2$cum.pct.repaid >= .1)])
ps$reached <- ifelse(is.infinite(ps$reached), NA, ps$reached)

output <- data.frame(GlobalClientID = unique(x$GlobalClientID),
percent.repaid.june = unique(june$pct.repaid), 
percent.repaid.may = unique(may$pct.repaid),
percent.repaid.april = unique(april$pct.repaid),
 percent.repaid.pp = unique(pp$pct.repaid),
  percent.repaid.pp2 = unique(pp2$pct.repaid),
  percent.repaid.pp3 = unique(pp3$pct.repaid),
  total.credit = unique(x$TotalCredit),
  average.prepay.amount = unique(ps$average),
  number.prepays = unique(ps$num),
  first.payment.week = unique(ps$first),
  avg.days.btwn.pay = unique(ps$avg.days.btwn.pay),
  reached.pre = unique(ps$reached),
  GL = unique(x$Facilitator))

    return (output)
})
)

#50 warning is OK

#checks
table(re15$number.prepays[is.na(re15$average.prepay.amount)])
table(re15$reached.pre[is.na(re15$average.prepay.amount)])
table(re15$reached.pre[is.na(re15$average.prepay.amount)])

#add new vs returning

re15$vet <- match(re15$GlobalClientID, uganda_2014LR$GlobalClientID)

#recategorize variables
re15$vet <- ifelse(is.na(re15$vet), 0, 1)

#add dropped

dropped <- subset(uganda_dropped2015, select = (c(GlobalClientID, Solar.qty, OAFID)))
dropped$drop <- 1

#add solar
solar <- subset(uganda_2015LR, select = (c(GlobalClientID, Solar.qty, OAFID)))
solar$drop <- 0

other <- rbind(dropped,solar)

re15 <- merge(re15, other, by = "GlobalClientID", all = T)

#lets do 2014

d <- subset(uganda_dropped2014, select = c(GlobalClientID,TotalCredit, Facilitator))
d$dropped2 <- 1
c <- subset(uganda_2014LR, select = c(GlobalClientID,TotalCredit, Facilitator))
c$dropped2 <- 0
cd <- rbind(c, d)

u4 <- merge(u3, cd, by = "GlobalClientID", all.y = T)

u4$month <- month(u4$date)

u4$Facilitator <- as.numeric(ifelse(u4$Facilitator == "True", 1, 0))

#lets  vectorize
   #   test2 <- split(u4, u4$GlobalClientID)
#    x <- test2[[271]]
re14 <- do.call(rbind, lapply(split(u4, u4$GlobalClientID),
  function(x) {
  
#percent repaid for april, may, june  
    april <- x
    
    april$value <- ifelse(april$date < as.Date("2014-05-01"), april$Amount, 0)
    
    april$sum <- ifelse(april$date >= as.Date("2014-05-01"), 0, sum(april$value))
 april$pct.repaid <- ifelse(april$sum == 0| april$TotalCredit == 0, 0, april$sum/april$TotalCredit)
 april$pct.repaid <- max(april$pct.repaid)
    
    may <- x
    
    may$value <- ifelse(may$date < as.Date("2014-06-01"), may$Amount, 0)
    may$sum <- ifelse(may$date >= as.Date("2014-06-01"), 0, sum(may$value, na.rm = T))
 may$pct.repaid <- ifelse(may$sum == 0| may$TotalCredit == 0, 0, may$sum/may$TotalCredit)
 may$pct.repaid <- max(may$pct.repaid)
    
    june <- x
    
    june$value <- ifelse(june$date < as.Date("2014-07-01"), june$Amount, 0)
    #4 0s from season clients who had not repaid

        june$sum <- ifelse(june$date >= as.Date("2014-07-01"), 0, sum(june$value, na.rm = T))
 june$pct.repaid <- ifelse(june$sum == 0| june$TotalCredit == 0, 0, june$sum/june$TotalCredit)
 june$pct.repaid <- max(june$pct.repaid)

  #percent at prepayment
 
 #the soft deadline
 pp <- x
 
 pp$value <- ifelse(pp$date < as.Date("2014-03-01"), pp$Amount, 0)

    pp$sum <- ifelse(pp$date >= as.Date("2014-03-01"), 0, sum(pp$value))
 pp$pct.repaid <- ifelse(pp$sum == 0| pp$TotalCredit == 0, 0, pp$sum/pp$TotalCredit)
 pp$pct.repaid <- max(pp$pct.repaid)
 
   #percent at prepayment for that feb 15
 
 #speculating about some extended repayment
  pp2 <- x
 
 pp2$value <- ifelse(pp2$date < as.Date("2014-03-15"), pp2$Amount, 0)

    pp2$sum <- ifelse(pp2$date >= as.Date("2014-03-15"), 0, sum(pp2$value))
 pp2$pct.repaid <- ifelse(pp2$sum == 0| pp2$TotalCredit == 0, 0, pp2$sum/pp2$TotalCredit)
 pp2$pct.repaid <- max(pp2$pct.repaid)

  pp3 <- x
 
 pp3$value <- ifelse(pp3$date < as.Date("2014-04-15"), pp3$Amount, 0)

    pp3$sum <- ifelse(pp3$date >= as.Date("2014-04-15"), 0, sum(pp3$value))
 pp3$pct.repaid <- ifelse(pp3$sum == 0| pp3$TotalCredit == 0, 0, pp3$sum/pp3$TotalCredit)
 pp3$pct.repaid <- max(pp3$pct.repaid)
 
#average size of payment in prepayment
 #using whole prepayment period

  ps <- x
 ps$pre <- ifelse(ps$date <= as.Date("2014-03-15"), ps$Amount, 0)
  ps$pre[ps$pre == 0] <- NA
  ps$average <- mean(ps$pre, na.rm = T)
  
 #number of payments
ps$num <- length(which(!is.na(ps$pre)))

 #date of first payment
#we got this in week so each week paid will give a coefficient
ps$payday <- (as.Date(ifelse(ps$pre > 0, as.Date(ps$date), as.Date("2016-01-01"))))
 ps$first.date <- ifelse(ps$average > 0, min(ps$payday, na.rm = T), NA)
ps$first.date <- as.Date(ps$first.date)
ps$week <- as.numeric( format(ps$first.date+3, "%U"))
ps$first <- ifelse(ps$week < 40, ps$week + 52, ps$week)

 #time between payments
time <- data.frame(unique(ps$payday))
names(time)[1] <- "date"
time <- time[order(as.Date(time$date, format="%d/%m/%Y")),]
time <- na.omit(time)

ps$avg.days.btwn.pay <- mean(diff(time))

#people paying more than once same date are creating NaN
#lets remove
ps$avg.days.btwn.pay <-ifelse(is.nan(ps$avg.days.btwn.pay) == T, NA, 
ps$avg.days.btwn.pay)

 #week they reached prepayment
 time2 <- subset(ps, select = (c(date,Amount, TotalCredit)))
time2 <- time2[order(as.Date(time2$date, format="%d/%m/%Y")),]
time2$week <- as.numeric( format(time2$date+3, "%U"))
time2$week <- ifelse(time2$week < 40, time2$week + 52, time2$week)
time2$cum <- cumsum(time2$Amount)
time2$cum.pct.repaid <- (time2$cum/time2$TotalCredit)
ps$reached <- min(time2$week[which(time2$cum.pct.repaid >= .1)])
ps$reached <- ifelse(is.infinite(ps$reached), NA, ps$reached)


output <- data.frame(GlobalClientID = unique(x$GlobalClientID),
percent.repaid.june = unique(june$pct.repaid), 
percent.repaid.may = unique(may$pct.repaid),
percent.repaid.april = unique(april$pct.repaid),
 percent.repaid.pp = unique(pp$pct.repaid),
  percent.repaid.pp2 = unique(pp2$pct.repaid),
  percent.repaid.pp3 = unique(pp3$pct.repaid),
  total.credit = unique(x$TotalCredit),
  average.prepay.amount = unique(ps$average),
  number.prepays = unique(ps$num),
  first.payment.week = unique(ps$first),
  avg.days.btwn.pay = unique(ps$avg.days.btwn.pay),
  reached.pre = unique(ps$reached),
  GL = unique(x$Facilitator))

    return (output)
})
)

#50 warning is OK

#checks
table(re14$number.prepays[is.na(re14$average.prepay.amount)])
table(re14$number.prepays[is.na(re14$first.payment.week)])
table(re14$number.prepays[is.na(re14$reached.pre)])

test3 <- subset(re14, is.na(re14$avg.days.btwn.pay))
test3 <- subset(test3, !is.na(test3$average.prepay.amount))

which(re14$GlobalClientID == "fd9e6d90-8690-47b9-b5b9-d10e0b16edb0")

#days between pay is usually NA from having one payment

#add dropped

dropped <- subset(uganda_dropped2014, select = (c(GlobalClientID, Solar.TU.qty, OAFID)))
dropped$drop <- 1

#add solar
solar <- subset(uganda_2014LR, select = (c(GlobalClientID, Solar.TU.qty, OAFID)))
solar$drop <- 0

other <- rbind(dropped,solar)

re14 <- merge(re14, other, by = "GlobalClientID", all = T)