#####
library(ipumsr)
library(tidycensus)
library(magrittr)
library(tidyverse) 
library(skimr) 
library(broom) 
library(car)
library(estimatr)
library(tidyr)
library(tibble)
library(dplyr)
library(readxl)
library(modelsummary)

#####

#Load Data from CPS

election_df <- read_excel("Copy of Governor Elections.xlsx")

ddi <- read_ipums_ddi("cps_00014.xml")
df <- read_ipums_micro(ddi)

##Clean Data for Analysis 

#Filter Data 
df <- df %>% drop_na(YEAR,EDUC,SEX,RACE,INCWAGE,AGE)

df %<>% filter(EDUC>=070)
df %<>% filter(EDUC<999)

df %<>% filter(INCWAGE>=1)
df %<>% filter(INCWAGE<99999999)


df %<>% filter(STATEFIP<=56)





#RACE Variable 


df %<>% mutate(RACENW = case_when(
  RACE==100 ~ 1,
  RACE==200 ~ 2,
  RACE==300 ~ 2,
  RACE==650 ~ 2,
  RACE==651 ~ 2,
  RACE==652 ~ 2,
  RACE==700 ~ 2,
  RACE==801 ~ 2,
  RACE==802 ~ 2,
  RACE==803 ~ 2,
  RACE==804 ~ 2,
  RACE==805 ~ 2,
  RACE==806 ~ 2,
  RACE==807 ~ 2,
  RACE==809 ~ 2,
  RACE==810 ~ 2,
  RACE==811 ~ 2,
  RACE==812 ~ 2,
  RACE==813 ~ 2,
  RACE==814 ~ 2,
  RACE==815 ~ 2,
  RACE==816 ~ 2,
  RACE==817 ~ 2,
  RACE==818 ~ 2,
  RACE==819 ~ 2,
  RACE==820 ~ 2,
  RACE==830 ~ 2))

##Transform Variables to Binary variables

#Race
#0 if White Only, 1 if Nonwhite
df %<>% mutate(RACENW = case_when(
  RACENW==1 ~ 0,
  RACENW==2 ~ 1))

#Mutate gender 
# 0 if Male and 1 if Female 
df %<>% mutate(SEX = case_when(
  SEX==1 ~ 0,
  SEX==2 ~ 1))

#Mutate Schooling Years
#create uniformity in schooling 
#14 years = 12th grade/HS Diploma
#24 years = Doctorate or Professional School Degree

df %<>% mutate(EDUCYRS = case_when(
  EDUC==070 ~ 14,
  EDUC==071 ~ 14,
  EDUC==073 ~ 14,
  EDUC==080 ~ 15,
  EDUC==081 ~ 15,
  EDUC==090 ~ 16,
  EDUC==091 ~ 16,
  EDUC==092 ~ 16))

df %<>% mutate(EDUCYRS = case_when(
  EDUC==100 ~ 17,
  EDUC==110 ~ 18,
  EDUC==111 ~ 18,
  EDUC==120 ~ 19,
  EDUC==121 ~ 19,
  EDUC==122 ~ 20,
  EDUC==123 ~ 20,
  EDUC==124 ~ 24,
  EDUC==125 ~ 24))


df %<>% mutate(years = case_when(
  YEAR==1994 ~ 1,
  YEAR==1995 ~ 2,
  YEAR==1996 ~ 3,
  YEAR==1997 ~ 4,
  YEAR==1998 ~ 5,
  YEAR==1999 ~ 6,
  YEAR==2000 ~ 7,
  YEAR==2001 ~ 8,
  YEAR==2002 ~ 9,
  YEAR==2003 ~ 10,
  YEAR==2004 ~ 11,
  YEAR==2005 ~ 12,
  YEAR==2006 ~ 13,
  YEAR==2007 ~ 14,
  YEAR==2008 ~ 15,
  YEAR==2009 ~ 16,
  YEAR==2010 ~ 17,
  YEAR==2011 ~ 18,
  YEAR==2012 ~ 19,
  YEAR==2013 ~ 20, 
  YEAR==2014 ~ 21,
  YEAR==2015 ~ 22,
  YEAR==2016 ~ 23,
  YEAR==2017 ~ 24,
  YEAR==2018 ~ 25))
  


#Drop data that is Not Available in Education Years column
df <- df %>% drop_na(EDUCYRS)


##Mutate Wage into a logarithm 
df <-  df %>% mutate(logincwage = log(INCWAGE))

##Mutate an additional variable of Age squared to see diminishing returns
df %<>% mutate(AGE.squared = AGE^2)

######

#Create State Vectors 

stabbvec <- c( "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
stnumvec <- as.integer(c("01", "02", "04","05","06", "08","09","10","12","13","15","16","17","18","19","20","21", "22", "23","24","25","26", "27", "28","29", "30", "31","32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"))

#as.integer

# Loop for Data Trial
#est <- lm(logincwage ~ SEX + RACE + EDUCYRS + AGE + I(AGE^2), 
#          data = df %>% filter(STATEFIP==stnumvec[[1]] & YEAR==1994))

#####

coefdf <- list()
# loop through all states and obtain gender wage gap in each year
for (i in seq(1,length(stabbvec))) 
  {
  for (t in seq(1994,2018)) 
    {
    print(paste0("state of ",stabbvec[i], " year is ", t))
    est <- lm(logincwage ~ SEX + RACENW + EDUCYRS + AGE + I(AGE^2), 
              data = df %>% filter(STATEFIP==stnumvec[[i]] & YEAR==t))
    temp <- tibble(YEAR=t, statenum = stnumvec[[i]], stateab=stabbvec[[i]], GWG = est$coefficients["SEX"])
    coefdf <- bind_rows(coefdf,temp)
  }
}



finaldf <- left_join(election_df,coefdf,by=c("year" = "YEAR","state"="stateab"))

#use = for governor data set 
#still do DiD model
# lab 15 /16 from econometrics 


est.fe.dem <- lm_robust(GWG ~ democrat, data = finaldf, fixed_effects = ~year + state)
est.fe.rep <- lm_robust(GWG ~ republican, data = finaldf, fixed_effects = ~year + state)


#GenderWage Gap Summary
modelsummary(list(est.fe.dem,est.fe.rep),output="markdown")
modelsummary(list(est.fe.dem,est.fe.rep),output="latex")


#####


est.simple <- lm(logincwage ~ SEX + RACENW + EDUCYRS + AGE + I(AGE^2), data=df)

summary(est.simple)
modelsummary(est.simple, output="markdown")
modelsummary(est.simple, output="latex")


est.simple2 <- lm(logincwage ~ SEX, data=df)
summary(est.simple2)
modelsummary(est.simple2, output="markdown")
modelsummary(est.simple2, output="latex")

#####
#Racial Wage Gap 

coefdf2 <- list()
# loop through all states and obtain gender wage gap in each year
for (i in seq(1,length(stabbvec))) 
{
  for (t in seq(1994,2018)) 
  {
    print(paste0("state of ",stabbvec[i], " year is ", t))
    est2 <- lm(logincwage ~ SEX + RACENW + EDUCYRS + AGE + I(AGE^2), 
              data = df %>% filter(STATEFIP==stnumvec[[i]] & YEAR==t))
    temp2 <- tibble(YEAR=t, statenum = stnumvec[[i]], stateab=stabbvec[[i]], RACEWG = est2$coefficients["RACENW"])
    coefdf2 <- bind_rows(coefdf2,temp2)
  }
}

finaldf2 <- left_join(election_df,coefdf2,by=c("year" = "YEAR","state"="stateab"))

est.fe.dem.race <- lm_robust(RACEWG ~ democrat, data = finaldf2, fixed_effects = ~year + state)
est.fe.rep.race <- lm_robust(RACEWG ~ republican, data = finaldf2, fixed_effects = ~year + state)


#RACIAL Wage Gap Summary
modelsummary(list(est.fe.dem.race,est.fe.rep.race),output="markdown")
modelsummary(list(est.fe.dem.race,est.fe.rep.race),output="latex")

#####

#1994-2000: Bill Clinton   - Democrat
coefdf_Clinton <- list()
# loop through all states and obtain gender wage gap in each year
for (i in seq(1,length(stabbvec))) 
{
  for (t in seq(1994,2000)) 
  {
    print(paste0("state of ",stabbvec[i], " year is ", t))
    est_Clinton <- lm(logincwage ~ SEX + RACENW + EDUCYRS + AGE + I(AGE^2), 
               data = df %>% filter(STATEFIP==stnumvec[[i]] & YEAR==t))
    temp_Clinton <- tibble(YEAR=t, statenum = stnumvec[[i]], stateab=stabbvec[[i]], GWG_Clinton = est_Clinton$coefficients["SEX"])
    coefdf_Clinton <- bind_rows(coefdf_Clinton,temp_Clinton)
  }
}

finaldf_Clinton <- left_join(election_df,coefdf_Clinton, by=c("year" = "YEAR","state"="stateab"))

est.fe.dem.Clinton <- lm_robust(GWG_Clinton ~ democrat, data = finaldf_Clinton, fixed_effects = ~year + state)
est.fe.rep.Clinton <- lm_robust(GWG_Clinton ~ republican, data = finaldf_Clinton, fixed_effects = ~year + state)


#GenderWage Gap Summary: Clinton President 
modelsummary(list(est.fe.dem.Clinton, est.fe.rep.Clinton), output="markdown")
modelsummary(list(est.fe.dem.Clinton, est.fe.rep.Clinton), output="latex")

#2001-2008: George W. Bush - Republican
coefdf_Bush <- list()
# loop through all states and obtain gender wage gap in each year
for (i in seq(1,length(stabbvec))) 
{
  for (t in seq(2001,2008)) 
  {
    print(paste0("state of ",stabbvec[i], " year is ", t))
    est_Bush <- lm(logincwage ~ SEX + RACENW + EDUCYRS + AGE + I(AGE^2), 
                      data = df %>% filter(STATEFIP==stnumvec[[i]] & YEAR==t))
    temp_Bush <- tibble(YEAR=t, statenum = stnumvec[[i]], stateab=stabbvec[[i]], GWG.Bush = est_Bush$coefficients["SEX"])
    coefdf_Bush <- bind_rows(coefdf_Bush,temp_Bush)
  }
}

finaldf_Bush <- left_join(election_df,coefdf_Bush,by=c("year" = "YEAR","state"="stateab"))

est.fe.dem.Bush <- lm_robust(GWG.Bush ~ democrat, data = finaldf_Bush, fixed_effects = ~year + state)
est.fe.rep.Bush <- lm_robust(GWG.Bush ~ republican, data = finaldf_Bush, fixed_effects = ~year + state)

#GenderWage Gap Summary: Clinton President 
modelsummary(list(est.fe.dem.Bush, est.fe.rep.Bush), output="markdown")
modelsummary(list(est.fe.dem.Bush, est.fe.rep.Bush), output="latex")

#2009-2016: Barack Obama   - Democrat
coefdf_Obama <- list()
# loop through all states and obtain gender wage gap in each year
for (i in seq(1,length(stabbvec))) 
{
  for (t in seq(2009,2016)) 
  {
    print(paste0("state of ",stabbvec[i], " year is ", t))
    est_Obama <- lm(logincwage ~ SEX + RACENW + EDUCYRS + AGE + I(AGE^2), 
                   data = df %>% filter(STATEFIP==stnumvec[[i]] & YEAR==t))
    temp_Obama <- tibble(YEAR=t, statenum = stnumvec[[i]], stateab=stabbvec[[i]], GWG.Obama = est_Obama$coefficients["SEX"])
    coefdf_Obama <- bind_rows(coefdf_Obama,temp_Obama)
  }
}

finaldf_Obama <- left_join(election_df,coefdf_Obama,by=c("year" = "YEAR","state"="stateab"))

est.fe.dem.Obama <- lm_robust(GWG.Obama ~ democrat, data = finaldf_Obama, fixed_effects = ~year + state)
est.fe.rep.Obama <- lm_robust(GWG.Obama ~ republican, data = finaldf_Obama, fixed_effects = ~year + state)

#GenderWage Gap Summary: Clinton President 
modelsummary(list(est.fe.dem.Obama, est.fe.rep.Obama), output="markdown")
modelsummary(list(est.fe.dem.Obama, est.fe.rep.Obama), output="latex")



#Discussion/Compare Bush v Obama Results
modelsummary(list(est.fe.dem.Bush, est.fe.rep.Bush, est.fe.dem.Obama, est.fe.rep.Obama), output="markdown")
modelsummary(list(est.fe.dem.Bush, est.fe.rep.Bush, est.fe.dem.Obama, est.fe.rep.Obama), output="latex")

#

vif(est.simple)
vif(est.simple2)

tidy(est.simple)
tidy(est.simple2)

tidy(est.fe.dem)
tidy(est.fe.dem.race)





















#GET GRAPH From St. Louis Federal Reserve Bank 

#Load Libraries 
library(ggplot2)
library(fredr)
library(quantmod)

#Use code to obtain 
getSymbols('LNS11300002',src='FRED')
#plot graph 
plot(LNS11300002)

data_WLF <-read.csv("LNS11300002.csv", header=TRUE)
head(data_WLF)

#give graph time frame, title and theme 
chartSeries(LNS11300002, subset='1972-01-01::2019-01-01',
            line.type = "l",
            name= "Women in Labor Force",
            theme=chartTheme('white'),
            TA="addVo()")











