#시각화 과정 유용한 규칙
#glimpse()함수로 데이터 구조 파악, 행, 변수 타입
#pairs() 산점도 행렬로 큰 그림을 본다. 이상한 점이나 흥미로운점이 없는지 살펴본다. 
#sample_n() 행의 수가 크면 랜덤으로 표본화 
#수량형 변수:히스토그램,geom_histogram(), 변수형 변수:막대그래프,geom_bar()
#두 변수간의 상관관계: geom_boxplot, geom_point()
#고차원의 관계 연구 제 3,4 변수는 geom_* 속성 추가, 많지 않은 경우엔 facet_wrap()함수 이용. 
#의미있는 변수명 사용 : xlab(), ylab(),labs()
#제목 추가:ggtitle()
#small multiple: 동시에 여러개 표 보여주기. facet_wrap(), facet_grid()함수: 함수 안의 좌/우 변수를 각각 행/열로 나눠 2차원으로 sub그래프를 그려준다. 
# 모든 데이터를 반드시 시각화
#dplyr능숙히 사용
library(dplyr)
##dplyr 유용한 유틸리티:glimpse, tbl_df(), %>%
i2 <- tibble::as_tibble(iris) 
class(i2)
#tbl_df 함수/클래스 , 행과열만 출력 
data <- tbl_df(read.csv("C:/Users/JohnKim_PC/OneDrive/문서/R/groups_1118.csv"))
data
class(data)

#glimpse: dataframe transpose 
glimpse(data)

#filter(df, 필터링 조건 )
filter(data, DEATH== 1)
# arrange, 행을 정렬 
arrange(data, YEAR, age_group)
# select 열을 선택
data %>% select(ros, DEATH)
# mutate 변수를 변환 
data<-data %>% mutate(end_date = as.Date(end_date, format= "%Y-%m-%d"), index_date=as.Date(index_date, format="%Y-%m-%d")) %>% mutate(duration=end_date-index_date)
# summarize(), min, max, mean, sum, sd, median, IQR, group_by
data %>% summarize(n_obs = n(), n_year = n_distinct(YEAR), n_QT_SCORE= n_distinct(QT_SCORE), med_qt= median(QT), max_qt =max(QT))
#random sample : 
sample_n(data, 10)
sample_frac(data, 0.01)
# distinct() :고유한 행을 찾아냄
distinct(select (data, YEAR))
distinct(select(data, drug))
data %>% select (drug) %>% distinct()
#group_by 
data %>% filter(DEATH==1) %>% group_by(ros ==1) %>% summarize(mean_age= median(age_group)) %>% arrange(-mean_age)
data %>% filter(DEATH ==1) %>% group_by(ros) %>% summarize (total= n())
# intersect(x,y):교집합, union(x,y) :합집합, setdiff(x,y):차집합. 
##histogram
summary(data$QT)
summary(data$age_group)
opar = par(mfrow=c(2,2))
hist(data$QT)
hist(data$age_group, nclass=50)
hist(sqrt(data$ros), nclass=50)

library(ggplot2)
install.packages("gapminder")
library(gapminder)
glimpse(gapminder)
##scatterplot
gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()
##ggplot example
df <- data.frame(gp=factor(rep(letters[1:3], each =10)), y =rnorm (30) )
glimpse(df)
ds <-df %>% group_by(gp) %>% summarize(mean= mean(y), sd=sd(y))
ds

## ex 1
ggplot(df, aes(x=gp, y=y)) + geom_point() + geom_point(data=ds, aes(y=mean),colour ="red", size=3)

ggplot(df) + geom_point(aes(x=gp, y=y)) + geom_point(data=ds, aes(x=gp, y=mean), colour ="blue", size=3)

ggplot() + geom_point(data=df, aes(x=gp, y=y)) + geom_point(data=ds, aes(x=gp, y=mean), colour ="blue",size=3) + geom_errorbar(data=ds, aes(x=gp, y=mean, ymin=mean-sd, ymax=mean +sd), colour="purple", width=0.4)

##ggplot & dplyr %>%
gapminder %>% ggplot(aes(lifeExp)) + geom_histogram()

##변수종류에 따른 시각화 기법
?diamonds
?mpg
glimpse(diamonds)
glimpse(mpg)
##수량형 변수:히스토그램, 이상점? 전반적인 분포모양, 어떻게 하면 종모양에 가까운가? 히스토그램이 너무 자세하거나 거칠지않은가?
library(gapminder)
library(ggplot2)
library(dplyr)
gapminder %>% ggplot(aes (x=gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% ggplot(aes (x=gdpPercap)) + geom_freqpoly() + scale_x_log10()
gapminder %>% ggplot(aes (x=gdpPercap)) + geom_density() + scale_x_log10()
summary(gapminder)
##범주형 변수 -막대그래프
diamonds %>% ggplot(aes(cut)) +geom_bar()
table (diamonds$cut)
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut))*100,1)
##dplyr 사용해서 통계량 계산 
##tally() = summarise(n=n())
diamonds %>% group_by(cut) %>% tally() %>% mutate(pct=round(n/sum(n)*100, 1))
?tally

## 두 수량형 변수 
#산점도(scatterplot ), 선형관계가 강한지, 약한지 확인. 
diamonds %>% ggplot(aes(carat, price)) + geom_point()
# 데이터 수가 너무 많을때는 alpha값을 줄여 점들을 좀더 투명하게 만든다. diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha=.1)

mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
#x,y좌표에 여러개의 중복된 관측치가 있을 때 geom_gitter()를 사용해  점들을 흩어준다. 
mpg %>% ggplot(aes(cyl, hwy)) +geom_jitter()

#데이터 개수가 너무 많을 땐 천 여개 정도릐 점들을 표본화한다.
#2개 이상의 연속변수를 다룸 산점도행렬 
pairs(diamonds %>% sample_n(1000))
glimpse(diamonds)

##수량형 변수(y)-범주형 변수 (x)
#hwy: highway miles per gallon, class : type of car
mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
?mpg

#geom_jitter로 점을 흩어주고, 레이어를 반투명하게 박스플롯
mpg %>% ggplot(aes(x=class, y=hwy)) + geom_jitter(col="grey") +geom_boxplot(alpha=.5)
#reorder로 순차적으로 class 나열 , reorder(정렬하고 싶은 변수(범주형), 연속형 데이터, function)
# 혹은 factor(levels=)로 수동으로 변수 순서 변경 
mpg %>% mutate(class=reorder(class, hwy, median )) %>% ggplot(aes(class, hwy)) + geom_jitter(col="grey") + geom_boxplot(alpha=.5)

# coord_flip() : x,y축 변경 
mpg %>% mutate(class=reorder(class, hwy, median )) %>% 
  ggplot(aes(class, hwy)) + geom_jitter(col="grey") + geom_boxplot(alpha=.5) + coord_flip()

# 두 범주형 변수 
# xtabs() 함수:분할표 , mosaicplot()
glimpse(data.frame(Titanic))
tbl_df(Titanic)
?xtabs
?Titanic
#xtabs:고차원 행렬 
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))
#고차원 행렬로 정리되면 모자익 플롯으로 시각화 
mosaicplot(Titanic, main="Survival on the Titanic", color=TRUE)
#어른과 아이중 누가 더 생존율이 높은가?- age변수와 생존(survival) 변수만 남기고 다 더함.
apply(Titanic, c("Age","Survived"), sum) #apply(object, direction, function to apply)
?apply
#비율, margin =1, 행별 데이터 총 합계 
round(prop.table(apply(Titanic, c("Age","Survived"), sum), margin=1), 3)

?ifelse
?sum
#group_by함수로 하면 좀더 명확. 
t2= data.frame(Titanic)
t2 %>% group_by(Sex) %>% summarize(n=sum(Freq), survivors=sum(ifelse(Survived=="Yes",Freq,0))) %>% mutate(rate_survival=survivors/n)
 
##geom 다른 속성들로 세분화
##2007년 평균소득과 기대수명간의 관계 +대륙별(범주형), 인구수(수량형)도 나타냄
##대륙별은 범주형변수라 색깔로 나타냄.인구수(pop)은 수량형 변수로 점크기로 나타냄
gapminder %>% filter (year==2007) %>% ggplot(aes(gdpPercap, lifeExp)) + geom_point(aes(size=pop, col=continent)) + scale_x_log10() +ggtitle("Gapminder data for 2007")
?geom_point

##facet_wrap()함수: 함수안의 ~오른쪽에 기재되는 변수별 level순서대로 sub그래프를 그려준다. 
#group=속성 별로 선이 그려짐 geom_line()
gapminder %>% ggplot(aes(year, lifeExp, group=country, col=continent))+ geom_line() + facet_wrap(~continent)                                  
?facet_wrap

#시각화 과정 유용한 규칙
#glimpse()함수로 데이터 구조 파악, 행, 변수 타입
#pairs() 산점도 행렬로 큰 그림을 본다. 이상한 점이나 흥미로운점이 없는지 살펴본다. 
#sample_n() 행의 수가 크면 랜덤으로 표본화 
#수량형 변수:히스토그램,geom_histogram(), 변수형 변수:막대그래프,geom_bar()
#두 변수간의 상관관계: geom_boxplot, geom_point()
#고차원의 관계 연구 제 3,4 변수는 geom_* 속성 추가, 많지 않은 경우엔 facet_wrap()함수 이용. 
#의미있는 변수명 사용 : xlab(), ylab(),labs()
#제목 추가:ggtitle()
#small multiple: 동시에 여러개 표 보여주기. facet_wrap(), facet_grid()함수: 함수 안의 좌/우 변수를 각각 행/열로 나눠 2차원으로 sub그래프를 그려준다. 
# 모든 데이터를 반드시 시각화
#dplyr능숙히 사용. 