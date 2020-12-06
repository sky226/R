install.packages("MatchIt")
install.packages("dplyr")
install.packages("comorbidity")
install.packages("demoGraphic")
install.packages("xml2")
install.packages("survival")
install.packages("survminer")
install.packages("moonBook")
install.packages("ggplot2")
install.packages("incidence")
install.packages("optmatch")
install.packages("lubridate")
install.packages("reshape")
library(reshape)
library(lubridate)
library(optmatch)
library(ggplot2)
library(MatchIt)
library(dplyr)
library(comorbidity)
library(demoGraphic)
library(survival)
library(survminer)
library(MASS)
library(moonBook)
library(incidence)

#data import 
getwd()
setwd("C:/github/R/R")
cci<- read.csv("C:/github/R/R/CCI_5YEAR_1204.csv")
data<-read.csv("C:/github/R/R/5year_obs_1204.csv")
data2<-read.csv("C:/github/R/R/group_1204.csv")
data[,c("KEY_SEQ","drug")] <-NULL
data2[,c("KEY_SEQ","drug")] <-NULL
str(data)
str(cci)
nrow(distinct(data, PERSON_ID)) #30062
nrow(distinct(cci, PERSON_ID)) #30062 
nrow(distinct(C, id)) #30062
C <- data.frame (id= cci$PERSON_ID, code = cci$SICK, stringAsFactors= FALSE)

print(head(C, n=15), row.names =FALSE )

# only 5_year stduy
# 사망일을 null값을 가장 큰 값으로 
nrow(data[data$ros ==1 & data$death==1,])# 32
nrow(data[data$ros ==0 & data$death==1,])#114
nrow(data[data$ros==1,]) #24724, 5338 
str(data)

# charcter -> date, death_date null값 채우기, study_period
data[,c("death_date","index_date","drop_date")]<- lapply( data[,c("death_date","index_date","drop_date")], function(x) as.Date(x,"%Y-%m-%d"))
data$death_date[is.na(data$death_date)] <-"9999-12-31"
data$study_period <- data$index_date + years(5)
str(data)
# real_end 
# min (drop_date, death_date, study_period(5))
test<- data[,c("PERSON_ID","drop_date","death_date","study_period")]
test2<- melt(test, id.vars="PERSON_ID", mesure.vars=c("drop_date","death_date","study_period"))
test3<-aggregate(test2$value, list(test2$PERSON_ID), min)
str(test3)

#null값이 있는지 확인.
test3[is.na(test3$x)==1,]
str(test3)
nrow(distinct(test3[!is.na(test3),], Group.1))
data <- left_join(data, test3, by=c("PERSON_ID"="Group.1")) 
str(data)

# death: 1, if death < x 
data$DEATH<- ifelse(data$death_date==data$x, 1, 0)
str(data)
nrow(data[(data$DEATH==1 & data$ros==1),]) #29
nrow(data[(data$DEATH==1 & data$ros==0),]) #100
colnames(data)
str(data)
data[,c("death_date","drop_date","death","end_date","study_period")] <-NULL


# comorbidity MATCHING 
# charlson 
charlson <- comorbidity(x = C, id = "id", code = "code", score = "charlson", icd = "icd10", assign0 = FALSE)[,c("id", "ami", "rend", "cevd", "chf","msld","score")]
str(charlson) 

## elixhause
elixhauser <- comorbidity(x = C, id = "id", code = "code", score = "elixhauser", icd = "icd10", assign0 = FALSE)[,c("id", "carit", "valv", "hypunc","hypc","diabunc","diabc", "fed","hypothy","drug","alcohol","aids")] %>% mutate(htn = hypunc + hypc, dia= diabunc + diabc)
elixhauser[,c("hypunc","hypc","diabunc","diabc")] <- NULL
str(elixhauser) 

#ischemic, hemorrhage stroke
isc <-c("I63","G45")
hem <-c("I60","I61","I62")
stroke <- cci %>% mutate( isc= ifelse(SICK == isc, 1,0)) %>% mutate(hem = ifelse(SICK==hem,1,0)) 
str(stroke)
isc<- aggregate(stroke$isc, list(stroke$PERSON_ID),sum) %>% rename(c("Group.1"="PERSON_ID","x"="isc"))  #reshape packages 
str(isc) 
hem<- aggregate(stroke$hem, list(stroke$PERSON_ID), sum)%>% rename(c("Group.1"="PERSON_ID","x"="hem"))
str(hem)
str(data)
data <- left_join(data, charlson, by = c("PERSON_ID"="id"))
data <- left_join(data, elixhauser, by = c("PERSON_ID"="id"))
data <- left_join(data, hem, by=c("PERSON_ID"))
data <- left_join(data, isc, by=c("PERSON_ID"))
str(data)
remove(list=c("hem","isc","stroke","test","test2","test3","test4","data2"))
remove(list=c("isc","hem","charlson","elixhauser","C","cci","isc2"))
gc()

##na 확인
nrow(distinct(data[!is.na(data),], PERSON_ID))
str(data)
data<-data %>% rename(c("ros" = "is_case", "DEATH"="is_event","x"="end_date"))
str(data)
nrow(data[(data$is_event==1 & data$is_case==1),]) #32--> 29 
nrow(data[(data$is_event==1 & data$is_case==0),]) # 100
data[is.na(data$end_date)==1]
data[is.na(data)] <- 0
nrow(data[data$is_case==1,]) #5492(1~9) / 5338(5year)
nrow(data[data$is_case==0,]) # 25754 / 24724

##ps matching 

with(data,tapply(is_case,is_event, mean)) #case별로 평균 사망은?
str(data)
#data<-data[is.na(data)]
tail(data)
#check missing freq 
table(is.na(data))
data<-na.omit(data)
str(data)
matched <- matchit(is_case ~ sex + age_group + QT + QT_SCORE + ami + rend + cevd + chf+ msld + score + carit +valv + htn + dia+ fed + hypothy + drug + alcohol +aids + hem + isc,  method="nearest", data=data, ratio=1)

case <- match.data(matched, group = 'treat', subclass = "subclass")
control <- match.data(matched, group = 'control', subclass = "subclass")
a <- data.frame(matched$match.matrix) # CASE, CONTROL ROWNUMBER?
case$subclass <- rownames(case) #점수별로 
control <- control[a[!is.na(a)],] # ALL ID IN CONTROL 
control$subclass <- which(!is.na(a))

total <- rbind(case,control) %>%
  mutate(index_date = as.Date(index_date, format="%Y-%m-%d"), censored_day = as.Date(end_date, format="%Y-%m-%d")) %>%   mutate(duration = censored_day - index_date) %>% mutate(duration_year = round(duration/365,1))
str(case)
str(total)
total[,c("end_date","index_date","duration","censored_day")] <- NULL
#remove(list=("total"))

#demographic
##n수
nrow(total[total$is_case == 1,]) #5492 / 5338
nrow(total[total$is_case == 0,]) #5492 / 5338
nrow(total[total$is_event == 1,]) #59 /58
nrow(total[total$is_event == 0,]) #10925 /10618
nrow(total[total$is_event==1 & total$is_case==1,]) #29
nrow(total[total$is_event==1 & total$is_case==0,]) #29

##category
#unique값이 5이하인것만 범주형 변수로
length(unique(total$htn))
select=sapply(total, function(x) length(unique(x)) <=5)
total[,select] <-lapply(total[, select], factor)
str(total)

var<-rownames(data.frame(select[select ==1]))[c(-1, -21)]
group_var = "is_case"
categorical_table = cat_table(var, group_var, total)
categorical_table$demo_table 

## numeric 
var = c("QT","age_group","score")
group_var = "is_case"
numeric_table = cont_table(var, group_var, total)
numeric_table$demo_table 

# ALL

all_var = rownames(data.frame(select))[c(4:25)]
group_var = "is_case"
all_table = demo_table(all_var, group_var, total)
all_table$demo_table
all_table$smd_table

mydocx(all_table$demo_table, "C:/github/R/R/5_yeartable_1206.csv")

## incidence rate 
ros<-total[total$is_case==1,]
ato<-total[total$is_case==0,]
mm.deaths<-sum (ros$is_event==1)#29(ros)
per.time<-as.integer(sum((ros$duration_year)))#12914
mortality.rate<-mm.deaths/per.time
round(100*mortality.rate,1) # 0.2  

mm.deaths2<-sum (ato$is_event==1)#29 (ator)
per.time2<-as.integer(sum((ato$duration_year)))#16793
mortality.rate2<-mm.deaths2/per.time2
round(100*mortality.rate2,1) # 0.1
remove(list=c("ato","case","control","matched","ros","stroke","numeric_table","categorical_table","df","data2","data_nomiss","d","CCI","cci","a","ase","charlson","elixhauser"))
gc()
#survival packages
max(total$duration_year)
survival.object <-Surv(total$duration_year, total$is_event ==1)
?Surv
##kaplan-meier
KM.object<-survfit(survival.object ~total$is_case)
KM.object
##log-rank like chi square to compare two curves
##(Σ(Oij − E ij)^2 / var(Σ(Oij − E ij)))
surv.p <- survdiff(survival.object ~ total$is_case)
surv.p

plot(KM.object, fun= function(x){1-x},mark.time=T, ylab="Cumulative death per 1 million prescription", xlab="year",col=c("black","red"), yaxt="n")
lines(KM.object, col=1:2, lty=2:3)
my.axis <-axTicks(2)*1000000
axis(side=2, at= axTicks(2), labels= my.axis)
text(1, 0.009, paste("p-value:", round(1-pchisq(surv.p$chisq,1),3)))
legend("bottomright",legend = c('atorvastatin', 'rosuvastatin'),col=1:2, lty=2:3)

##lml plot
plot(KM.object, col=c("black","red"), fun="cloglog")

##proportional hazard
#cox-로수바스타틴만의 HR
str(total)
total1=na.omit(total)
#length(unique(table$col)) >= 2
total1$TS=Surv(total1$duration_year, total1$is_event ==1)
out <- coxph(TS ~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+ carit+ valv + fed + hypothy + alcohol+  htn + dia  , data = total1)
summary(out)
HRplot(out, show.CI=T, main="cox model with all variables")

##mycph
require(moonBook)
out2<- mycph(TS ~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+  carit+ valv + fed + hypothy +  alcohol+  htn + dia  , data = total1)
HRplot(out2, type=2, show.CI =T, main="Hazard ratios of all individual variables")

#data_cleansing
str(data)
unique(data$risk)

#AGE>=75
str(total)
total2<- total[total$age_group < 16, ]
out2= coxph(Surv(total2$duration_year, total2$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+  carit+ valv + fed + hypothy +  alcohol+  htn + dia +isc , data = total2)
summary(out2)
HRplot(out2, show.CI=T, main="cox model with all variables_risk group under 75")
#dose 

str(data)
str(total)
colnames(total)
head(total)
high<-c("454002ATB","502203ATB","502204ATB","11503ATB","11504ATB")
total<-total %>% mutate( dose= ifelse( last_drug == high, 1,0))
total3<- total[total$dose==0,]
out3= coxph(Surv(total3$duration_year, total3$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+  carit+ valv + fed + hypothy +  alcohol+  htn + dia  , data = total3)
summary(out3)
HRplot(out3, show.CI=T, main="cox model with all variables_risk group of low_dose")
# RISK Group Hr
# RISK:carit, fed , rend (심장부정맥, 전해질, 신질환)
#benefit: ami, cevd, isch, valv, chf
str(total)
hist(total$ami)
total4<-total[(total$carit==1|total$fed ==1 |total$rend ==1), ] 
out4= coxph(Surv(total4$duration_year, total4$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami+ cevd + chf+   valv +htn+dia+ hypothy +  htn + dia  , data = total4)
summary(out4)
HRplot(out4, show.CI=T, main="cox model with all variables of risk group")
str(total4)

## benefir group hr 
plot(total$isc)
total5<-total1[(total1$ami==1 | total1$cevd ==1 | total1$isc==1 |
                 total1$valv==1 | total1$chf ==1 ),]
out5= coxph(Surv(total5$duration_year, total5$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ rend +  carit+  fed + hypothy +  alcohol+  htn + dia , data = total5)
summary(out5)
HRplot(out5, show.CI=T, main="cox model with all variables of benefit group")
str(total5)

#Chronic disease: htn, dia
str(total)
plot(total$dia)
total6<-total[(total$htn==1 | total$htn ==2),]
out6= coxph(Surv(total6$duration_year, total6$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami+ rend + cevd+  chf+carit+ valv+  fed + hypothy +  alcohol + dia  , data = total6)
summary(out6)
HRplot(out6, show.CI=T, main="cox model with all variables of hypertension")

#dia 
total7<-total[(total$dia==1 | total$dia ==2),]
out7= coxph(Surv(total7$duration_year, total7$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami+ rend + cevd+  chf+carit+ valv+  fed + hypothy +  alcohol + htn  , data = total7)
summary(out7)
HRplot(out7, show.CI=T, main="cox model with all variables of diabet")

remove(list=c("out","out2","out3","out4","out5","total2","total3","total4","total5"))
gc()

remove(list=c("total6","total7","total1","C"))
gc()

remove(list=c("out6","out7","total","data"))
gc()

#mosaicplot
mosaicplot(total, main="survival on statin", color=T)
