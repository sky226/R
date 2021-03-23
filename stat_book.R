## 탐색적 자료분석 및 정규성 검정

# 자료입력 
depressiondata = read.csv("C:/Users/JohnKim_PC/Downloads/excersise/1_normality.csv")
attach (depressiondata)
depressiondata[(1:20),]

# 탐색적 자료분석
summary(score[group==1])
summary(score[group==2])
par(mfrow=c(2,3))
hist(score[group ==1], main="Group A")
hist(score[group ==2], main="Group B")
boxplot(score~group, col="yellow3")
stem(score[group ==1])
stem(score[group ==2])

#정규성 검정 p>0.05 정규성만족. 
shapiro.test(score[group ==1])
shapiro.test(score[group ==2])
qqnorm(score[group==1], col ="blue", main ="Normal Q-Q Plot of Group A")
qqline(score[group==1], col="red") ##qqline 추가. 
qqnorm(score[group ==2], col="blue", main ="Normal Q-Q Plot of Group B")
qqline(score[group ==2], col="red")

detach(depressiondata)

##독립표본 T검정
depressiondata = read.csv("C:/Users/JohnKim_PC/Downloads/excersise/2_independent_t_test.csv")
attach(depressiondata)
depressiondata[(1:20),]
boxplot(score~group, col="yellow")

#정규성검정
summary(score[group ==1])
summary(score[group ==2])
shapiro.test(score[group ==1])
shapiro.test(score[group ==2])

#등분산 검정 및 독립표본 t검정 시행
var.test(score~group)
t.test(score~group, var.equal=TRUE) #two sample t-test
t.test(score~group) #welch two sample t-test

detach(depressiondata)

##2. wilcoxon rank sum test 

#자료 입력 및 관찰
depressiondata = read.csv("C:/Users/JohnKim_PC/Downloads/excersise/2_wilcoxon_rank_sum_test.csv")
attach(depressiondata)
depressiondata[(1:3),]
boxplot(score~group, col="yellow")

#wilcoxon rank sum test 시행
wilcox.test(score~group, exact =FALSE)

detach(depressiondata)

#3. 대응표본 t검정
depressiondata = read.csv("C:/Users/JohnKim_PC/Downloads/excersise/3_paired_t_test.csv")
attach(depressiondata)
depressiondata[c(1:10),]
boxplot(pre, post, col="yellow")
segments(1, pre, 2, post, col =1, lwd=0.25)

#정규성 검정 후 paried T test 시행
shapiro.test(post-pre)
t.test(post-pre)

detach(depressiondata)

##wilcoxon signed rank test(치료 전후 )
#자료 입력 및 관찰
depressiondata =read.csv("C:/Users/JohnKim_PC/Downloads/excersise/3_wilcoxon_signed_rank_test.csv")
attach(depressiondata)
depressiondata
boxplot(pre,post,col="yellow")
segments(1,pre,2,post, col=1, lwd=0.25)
# wilcoxon signed rank test 시행 

wilcox.test(post-pre, exact =FALSE)
detach(depressiondata)
