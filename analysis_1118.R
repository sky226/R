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
cci<- read.csv("/home/yoonlab6/googleDrive/CCI_1204.csv")
data<-read.csv("/home/yoonlab6/googleDrive/group_1204.csv")
data[,c("KEY_SEQ","drop_date","death_date")] <-NULL
str(data)
str(cci)

C <- data.frame (id= cci$PERSON_ID, code = cci$SICK, stringAsFactors= FALSE)
print(head(C, n=15), row.names =FALSE )

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
isc<- aggregate(stroke$isc, list(stroke$PERSON_ID), sum) %>% rename(c("PERSON_ID"="Group.1","isc"="x"))
hem<- aggregate(stroke$hem, list(stroke$PERSON_ID), sum)%>% rename(c("PERSON_ID"="Group.1","hem"="x"))
str(hem)
data <- left_join(data, charlson, by = c("PERSON_ID"="id"))
data <- left_join(data, elixhauser, by = c("PERSON_ID"="id"))
data <- left_join(data, hem, by="PERSON_ID")
data <- left_join(data, isc, by="PERSON_ID")
str(data)
remove(list=c("isc","hem","charlson","elixhauser","C","cci"))
gc()

## 층화: risk, benefit  
data<-data %>% mutate( risk = carit + fed , benefit= ami + valv + cevd + chf + isc) %>% rename(c("is_case" = "ros", "is_event"="death"))

str(data)

data[is.na(data)] <- 0
nrow(data[data$is_case==1,]) #5492(1~9)
nrow(data[data$is_case==0,]) # 25754

##ps matching 

with(data,tapply(is_case,is_event, mean)) #case별로 평균 사망은?
str(data)
matched <- matchit(is_case ~ sex + age_group + QT + QT_SCORE + ami + rend + cevd + chf+ msld + score + carit +valv + htn + dia+ fed + hypothy + drug.y + alcohol +aids + hem + isc,  method="nearest", data=data, ratio=1)

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
total[,c("end_date")] <- NULL

#demographic
##n수
nrow(total[total$is_case == 1,]) #5492
nrow(total[total$is_case == 0,]) #5492
nrow(total[total$is_event == 1,]) #59
nrow(total[total$is_event == 0,]) #10925

##category
#unique값이 5이하인것만 범주형 변수로
length(unique(total$benefit))
select=sapply(total, function(x) length(unique(x)) <=5)
total[,select] <-lapply(total[, select], factor)
str(total)

var<-rownames(data.frame(select[select ==1]))[c(-1, -23)]
group_var = "is_case"
categorical_table = cat_table(var, group_var, total)
categorical_table$demo_table 

## numeric 
var = c("QT","age_group","score")
group_var = "is_case"
numeric_table = cont_table(var, group_var, total)
numeric_table$demo_table 

# ALL

all_var = rownames(data.frame(select))[c(6:29)]
group_var = "is_case"
all_table = demo_table(all_var, group_var, total)
all_table$demo_table
all_table$smd_table

mydocx(all_table$demo_table, "/home/yoonlab6/googleDrive/table_1205.csv")

## incidence rate 
ros<-total[total$is_case==1,]
ato<-total[total$is_case==0,]
mm.deaths<-sum (ros$is_event==1)#33(ros)
per.time<-as.integer(sum((ros$duration_year)))#15336
mortality.rate<-mm.deaths/per.time
round(100*mortality.rate,1) # 0.2  

mm.deaths2<-sum (ato$is_event==1)#26 (ator)
per.time2<-as.integer(sum((ato$duration_year)))#18719
mortality.rate2<-mm.deaths2/per.time2
round(100*mortality.rate2,1) # 0.1
remove(list=c("ato","case","control","matched","ros","a","depressiondata2","derpressiondata", "surv.p"))
gc()
#survival packages
str(total)
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
text(1, 0.013, paste("p-value:", round(1-pchisq(surv.p$chisq,1),3)))
legend("bottomright",legend = c('atorvastatin', 'rosuvastatin'),col=1:2, lty=2:3)

##lml plot
plot(KM.object, col=c("black","red"), fun="cloglog")

##proportional hazard
#cox-로수바스타틴만의 HR
str(total)
total1=na.omit(total)
#length(unique(table$col)) >= 2
total1$TS=Surv(total1$duration_year, total1$is_event ==1)
out <- coxph(TS ~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+ msld + carit+ valv + fed + hypothy + drug.y + alcohol+  htn + dia + hem + isc , data = total1)
summary(out)
HRplot(out, show.CI=T, main="cox model with all variables")

##mycph
require(moonBook)
out2<- mycph(TS ~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+  carit+ valv + fed + hypothy +  alcohol+  htn + dia  , data = total1)
HRplot(out2, type=2, show.CI =T, main="Hazard ratios of all individual variables")

#data_cleansing
str(data)
unique(data$risk)

# RISK Group Hr
plot(total1$risk)
total2<-total1[(total1$risk==1)| (total1$risk==2),]
out3= coxph(Surv(total2$duration_year, total2$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+  carit+ valv + fed + hypothy +  alcohol+  htn + dia  , data = total2)
summary(out3)
HRplot(out3, show.CI=T, main="cox model with all variables_risk group")

## benefir group hr 
plot(total1$benefit)
total3<-total1[total1$benefit==c(1,2,3,4),]
out4= coxph(Surv(total3$duration_year, total3$is_event ==1)~ is_case + sex +  QT+ QT_SCORE+ ami + rend + cevd + chf+  carit+ valv + fed + hypothy +  alcohol+  htn + dia  , data = total3)
summary(out4)
HRplot(out4, show.CI=T, main="cox model with all variables_benefit group")






