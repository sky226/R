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
cci<- read.csv("C:/Users/JohnKim_PC/OneDrive/문서/R/CCI_1118.csv")
data<-read.csv("C:/Users/JohnKim_PC/OneDrive/문서/R/groups_1118.csv")
data<-data [,c("PERSON_ID","index_date","DEATH","sex","age_group","ros","end_date","QT","QT_SCORE","over75")]

str(data)
str(cci)
head(cci)
head(data)

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
stroke<-cci %>% mutate( isc= ifelse(SICK ==isc, 1,0)) %>% mutate(hem = ifelse(SICK==hem,1,0)) 
isc<- aggregate(stroke$isc, list(stroke$PERSON_ID), sum) %>% rename(c("PERSON_ID"="Group.1","isc"="x"))
hem<- aggregate(stroke$hem, list(stroke$PERSON_ID), sum)%>% rename(c("PERSON_ID"="Group.1","hem"="x"))
str(hem)
data <- left_join(data, charlson, by = c("PERSON_ID"="id"))
data <- left_join(data, elixhauser, by = c("PERSON_ID"="id"))
data <- left_join(data, hem, by="PERSON_ID")
data <- left_join(data, isc, by="PERSON_ID")

data[, "x"]<-NULL
str(data)
remove(list=c("isc","hem","charlson","elixhauser","C","cci"))
gc()

## 층화: risk, benefit  
data<-data %>% mutate( risk = carit + fed , benefit= ami + valv + cevd + chf + isc + hem) %>% rename(c("is_case" = "ros", "is_event"="DEATH"))

head(data)
str(data)

data[is.na(data)] <- 0
nrow(data[data$is_case==1,]) #5333
nrow(data[data$is_case==0,]) # 24723

##ps matching 

with(data,tapply(is_case,is_event, mean)) #case별로 평균 사망은?

matched <- matchit(is_case ~ sex + age_group + QT + QT_SCORE + ami + rend + cevd + chf+ msld + score + carit +valv + htn + dia+ fed + hypothy + drug + alcohol +aids ,  method="nearest", data=data, ratio=1)

case <- match.data(matched, group = 'treat', subclass = "subclass")
head(case)
control <- match.data(matched, group = 'control', subclass = "subclass")
str(case$subclass)
a <- data.frame(matched$match.matrix)
case$subclass <- rownames(case) #점수별로 
control <- control[a[!is.na(a)],]
control$subclass <- which(!is.na(a))

total <- rbind(case,control) %>%
  mutate(index_date = as.Date(index_date, format="%Y-%m-%d"), censored_day = as.Date(end_date, format="%Y-%m-%d")) %>%
  mutate(duration = censored_day - index_date) %>%

str(total)
total[,c("end_date")] <- NULL

#demographic
##n수
nrow(total[total$is_case == 1,]) #5333
nrow(total[total$is_case == 0,]) #5333
nrow(total[total$is_event == 1,]) #48
nrow(total[total$is_event == 0,]) #10618

##category
#unique값이 3이하인것만 범주형 변수로
str(data)
l1<-colnames(total)
l2<- l1[c(1,2,7,14,23,24,25,26,27,28)]
l1 <- l1[-c(1,2,7,14,23,24,25,26,27,28)]

total <- total %>% mutate_each_(funs(factor), l1) %>% mutate_each_(funs(as.numeric),l2)
str(total)
var<-l1[-4]
group_var = "is_case"
categorical_table = cat_table(var, group_var, total)
categorical_table$demo_table 

## numeric 
var = "QT"
group_var = "is_case"
numeric_table = cont_table(var, group_var, total)
numeric_table$demo_table 

# ALL
l1[19]="QT"
all_var = l1[-4]
group_var = "is_case"
all_table = demo_table(all_var, group_var, total)
all_table$demo_table
all_table$smd_table

mydocx(all_table$demo_table, "C:/Users/JohnKim_PC/OneDrive/문서/R/demographic_table3.csv")

## incidence rate 
ros<-total[total$is_case==1,]
ato<-total[total$is_case==0,]
mm.deaths<-sum (ros$is_event==1)#48-30(ros), 18(ator)
per.time<-sum((ros$duration)/365)#26015.75-> 12104.46(ros), 13911.29(ato)
mortality.rate<-mm.deaths/per.time
round(100*mortality.rate,1) # 0.2  0.2case per 100py(ros), 0.1(ato) 

#survival packages
str(total)
total<-total %>% mutate(duration_year = duration/365)
survival.object <-Surv(total$duration_year, total$is_event ==1)

##kaplan-meier
KM.object<-survfit(survival.object ~total$is_case)
summary(KM.object)
plot(KM.object, data=total,  fun="cumhaz",mark.time=F, ylab="cumulative hazard", xlab="year",col=c("black","red"))
legend("bottomright",legend = c('atorvastatin', 'rosuvastatin'),col=1:2, lty=1:2)

ggsurvplot(KM.object, data = total, pval = TRUE, fun='cumhaz', legend.labs = c('control', 'treated'))

#compare 2 groups
KM.object.ros<-survfit(survival.object ~ total$is_case)
plot(KM.object.ros, conf.int=T, col=c("black","red"))
legend("bottomright", pch=1, col=c("black","red"), legend=c("ros", "ator"))

##log-rank like chi square to compare two curves
##(Σ(Oij − E ij)^2 / var(Σ(Oij − E ij)))
survdiff(survival.object ~ total$is_case)
##lml plot
plot(KM.object.ros, col=c("black","red"), fun="cloglog")

##proportional hazard
## variance -> as.numeric
l1<-colnames(total)
l1<- l1[c(3,5,6,14,20,21,22)]
total_cox <- total %>% mutate_each_(funs(as.numeric), l1)
#cox-로수바스타틴만의 HR
out1=coxph(Surv(duration_year, is_event==1)~is_case, data=total_cox)
out1
##COX-다른 변수들도 포함. 
out2= coxph(TS ~.-is_event-PERSON_ID-index_date-distance-subclass-censored_day-duration-duration_year, data = total_cox)
final=step(out2,direction="backward")
summary(final)
HRplot(final,type=3, show.CI=T,main="final model selected by backward elimination")

#COX-간략히 
cox.object<- coxph(Surv(duration_year, is_event==1) ~ is_case + sex +  QT  + over75 + risk_qt + benefit + chronic, data = total_cox )
summary(cox.object)
str(total_cox)
HRplot(cox.object, show.CI = T, main="Cox model with all variables")


#층화 stratify-sex
fit<- coxph(Surv(duration_year, is_event==1) ~ is_case + strata(sex), data = total_cox )
summary(fit)
fit2<- coxph(Surv(duration_year, is_event==1) ~ is_case + strata(chronic), data = total_cox )
summary(fit2)
## Seperate models for female and male , over 75, risk_qt, beneift, chronic

## benefirc, chronic ; factor-> numeric
total$benefit1<- cut(total$benefit, breaks=c(1,3, 6),
                                      include.lowest=T,
                                      right=F, labels=c("medium","high"))
total$chronic1<- cut(total$chronic, breaks=c(1,3, 7),
                     include.lowest=T,
                     right=F, labels=c("medium","high"))
fit_<-lapply(split(total, total$chronic1 ), 
                FUN=function(total){
                  coxph(Surv(duration_year,is_event==1) ~is_case ,total)
                  })
str(total)

##moonbook
library(moonBook)
total_cox$TS=Surv(total_cox$duration_year, total_cox$is_event ==1)
out= mycph(TS ~.-is_event-PERSON_ID-index_date-distance-subclass-censored_day-duration-duration_year-QT_SCORE-age_group, data = total_cox)
out
HRplot(out, type=2, show.CI=TRUE,  main="Hazard ratios of all individual varaibles")
HRplot(out, type=3, show.CI=TRUE, sig.level=0.05, main="Hazard ratios of significant varaibles")
str(total_cox)
str(total)
