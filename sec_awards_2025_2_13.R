# sec_awards_2025_x_y.R 
# data on SEC awards to whistleblowers 2021-2024 for a paper titled: "Whistleblowers and Financial Fraud."

# packages used
#library(ggplot2); theme_set(theme_bw())
#library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables
library(dplyr)# sorting by group (by year)

# reading SEC award data from an Excel file
library(readxl)
setwd("~/Papers/fraud/fraud_R_Derive")
dir()
a1.df = data.frame(read_excel("sec_awards.xlsx"))
typeof(a1.df)
colnames(a1.df)
#
# Deleting all by columns 1 to 3
a2.df = as.data.frame(a1.df[,1:3])
colnames((a2.df))
dim(a2.df)
head(a2.df, 100)
#
# Adding a column by which replaces "shared among: 2 and more" with 3. Assumption to be stated in the paper
class(a2.df)
typeof((a2.df))
str(a2.df)
a3.df = a2.df
a3.df$shared_among2 = with(a2.df, ifelse(Shared.among=="2 or more", 3, Shared.among))
head(a3.df, 50)

# Adding column "year"
a4.df = a3.df
a4.df$Date.awarded = as.Date(a4.df$Date.awarded)
head(a4.df)
str(a4.df)
# Construct column "year"
a4.df$year = a4.df$Date.awarded
a4.df$year = format(as.Date(a4.df$year, format="%Y-%m-%d"), "%Y")
str(a4.df)

# delete Shared.among (original) and make share.among2 numerical
a5.df = subset(a4.df, select = -c(Shared.among))
str(a5.df)
# making shared_among2 numeric
a5.df$shared_among2 = as.numeric(a5.df$shared_among2)
str(a5.df)
# making year numeric
a5.df$year = as.numeric(a5.df$year)
str(a5.df)

# delete Data.awarded
a6.df = subset(a5.df, select = -c(Date.awarded))
str(a6.df)

# shortening column  name Amount..million
names(a6.df)[names(a6.df)=="Amount..million"] = "amount"
names(a6.df)
# shortening column  name "shared_among2"
names(a6.df)[names(a6.df)=="shared_among2"] = "shared"
names(a6.df)

### Building several data frame with data by year. Then, I will combine them into the final table. 
a7.df = a6.df
str(a7.df)

# maximum awarded by year
(max_amt.df = a7.df %>% group_by(year) %>% summarise(highest = max(amount)))

# number frauds by year
(total_fraud.df = a7.df %>% group_by(year) %>% summarise(frauds = n()))

# total $ awarded by year
(total_amt.df = a7.df %>% group_by(year) %>% summarise(total = sum(amount)))

# start inputting the above to final data frame (more to be added below)
(sec1.df = max_amt.df)
(sec2.df = cbind(sec1.df, total_amt.df$total))
colnames(sec2.df)[colnames(sec2.df)=="total_amt.df$total"] = "total"
sec2.df

# adding number of fraud to final table
(sec3.df = cbind(sec2.df, total_fraud.df$frauds))
colnames(sec3.df)[colnames(sec3.df)=="total_fraud.df$frauds"] = "cases"
sec3.df

# avg total $ per fraud by year
sec3.df$total_per_case = sec3.df$total/sec3.df$cases
sec3.df

# number awards by year
(total_awarded.df = a7.df %>% group_by(year) %>% summarise(awards = sum(shared)))
#
sec4.df = sec3.df
sec4.df = cbind(sec4.df, total_awarded.df$awards)
sec4.df
colnames(sec4.df)[colnames(sec4.df)=="total_awarded.df$awards"] = "awards"
sec4.df

# Award per fraud case
sec4.df$awards_per_case = sec4.df$awards/sec4.df$cases
sec4.df

### Preparing LaTeX table
sec5.df = sec4.df
str(sec5.df)

(sec_digitsm = matrix(c(rep(0,13), rep(0, 13), rep(2,13), rep(3,13), rep(0,13), rep(3,13), rep(0,13),  rep(3,13)),  nrow = 13, ncol = 7+1, byrow = F))
dim(sec_digitsm)
#
print(xtable(sec5.df, digits = sec_digitsm), include.rownames = F, hline.after = c(0))

