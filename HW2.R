library(ggplot2)
diamonds
library(knitr)
kable(diamonds)
#1:
#the price is ratio and quantitative
getmode=function(v){
  dfv=as.data.frame(table(v))
  print(dfv)
  print(which.max(dfv[,2]))
  dfv[which.max(dfv[,2]),1:2]
  
}
#?????????? ??????????
getmode(diamonds$price)
#the price 605 has the most frequency
#???????????? ???????? ?????? ???????? 
IQR(diamonds$price)
var(diamonds$price)
sd(diamonds$price)
max(price)
min(price)
#?????? ???????? ?? ???????? ???????????? ???????? ????????
fivenum(diamonds$price)
boxplot(diamonds$price,horizontal=T)
#2
hist(diamonds$price,
     xlab='price',
     main='Histogram of Price',
     breaks=30,
     col='darkorange',
     border='blue',
     freq=TRUE)
#this defnitly doesn't have normal distribution.
#3
#the color is qualitative and nominal
attach(diamonds)
coltable=as.data.frame(table(color))
coltable[which.max(coltable[,2]),1]
#not all the color types are used and the color "G" is the most frequent.
mysum=sum(coltable["Freq"])
mycolor=table(color)
newcolor=mycolor[]/mysum
newcolor
barplot(newcolor,legend.text = T,col=rainbow(7),ylab="percentage of Frequnecy",xlab="color",main="bar plot of color")
#bonus
as.data.frame(table(depth))
as.data.frame(table(clarity))
diamonds
clarity[which.max(depth)]
#the VS2 clarity has the most depth 
table(clarity,depth)
barplot(table(clarity,depth),legend.text = T,col=blues9,xlab='depth',ylab='fequently')
#it seems like the relation between depth and frequency has a normal distribution.
#4
diamonds
cut[which.max(depth)]
#the Fair cut has the most depth 
barplot(table(cut,depth),legend.text = T,col=blues9,xlab='depth',ylab='frequently')
s1=depth[cut=="Fair"]
s2=depth[cut=="Good"]
s3=depth[cut=="Very Good"]
s4=depth[cut=="Premium"]
s5=depth[cut=="Ideal"]
boxplot(s1,s2,s3,s4,s5,Horizontal=T,xlab='cut')
#it seems like the relation between depth and frequency has a normal distribution.up until the depth is around average
#with increase in depth,we have more cut levels and from average to max depth this decrease.
#5
hist(price[cut=="Fair"],breaks=50,col=rgb(1,0,0,0.5),xlim=c(0,8000))
hist(price[cut=="Good"],breaks=50,col=rgb(0,0,1,0.5),add=T)
hist(price[cut=="Premium"],breaks=50,col=rgb(0,1,1,0.5),add=T)
hist(price[cut=="Ideal"],breaks=50,col=rgb(0,1,0,0.2),add=T)
legend("topright",legend=c("Fair","Good","Premium","Ideal"),col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5),rgb(0,1,1,0.5),rgb(0,1,0,0.2)),
      pt.cex=2,pch=15)
#analysis:???
#6
diamonds
color
#contigency Table
table(color,cut)
#it's obvious the better the cut degree gets, the more frequent is the use of that color.
barplot(table(cut,color),col=rainbow(7),legend.text = T)
#in this bar plot,we can see that there is more frequency of each color at Ideal cut and it decrease as
#Cut degrees getting to basic levels.
barplot(table(color,cut),col=rainbow(7),legend.text = T)
#as it shows , all the degree of cuts include all 7 color but the frequency of color in 
#higher level of cut is more than basic level.
#7
diamonds
ggplot(data=diamonds)+
geom_point(mapping=aes(x=price,y=carat))
#as it's shown in the plot, the density near the price 0 to 10000 is too much bt it's almost steady with 
#the rate of carat between 0 to less than 2. as the price goes up and carat increase, the density lowers down and we can have visible
#scatter plots. so with the higher price and carat between 0 to 2, the density is high.
newprice=price[price<=1000]
table(newprice)
newcarat=carat[price<=1000]
#table(newcarat)
diamonds
newdiamonds=subset(diamonds,price<=1000)
ggplot(data=newdiamonds)+
geom_point(mapping=aes(x=newprice,y=newcarat),size=1)
#now with this new data consists of only prices less than 1000, we have a less dense plot, as it is shown, the density
#in parts where carat is 0.2 to 0.4 is stable and a lot more than other parts.as price goes higher and  so does the carat
#the density and frequency lower down.we can conclude that in almost any price less than 1000 , when the carat is between
# 0.2 to 0.45 , the frequency is more than other time and it's stable.
####using jitter
ggplot(data=newdiamonds)+
  geom_point(mapping=aes(x=newprice,y=newcarat),size=1,position = 'jitter')
#?jitter how to use it?
#8
diamonds
par(mfrow=c(1,1))
coplot(price~carat|cut,rows=1)
#almost in every cut degree, there is high density of diamonds with price between 0 to 10000 and carat between 0 to 3.
#at some cut degree such as Fair and Good the density in lower prices is higher than higher prices whereas in very Good , Premium 
# and Ideal cut the density between carat 0 to 3 is high in almost every price.
plot(price~carat,
     xlab = "carat",
     ylab = "price",
     main = "histogram",
     pch  =20,
     cex  = 1,
     col  = factor(cut, labels = 1:5))
legend("bottomright",
       legend = levels(factor(cut)),
       pch  = 20,
       col  = 1:5)
#this plot like the previous one shows high density of three high cut degree "Very good(green)","Premium(dark blue)",
#"Ideal(light blue)" in almost each price in carat in the range of 0 to less than 3. also cut such as "Good" and "Fair"
#are in higher density when the price is lower than 10000 , the same result as "coplot".
#9
diamonds
coplot(price~carat|clarity,rows=1)
#as it is shown in the chart, the density of diamonds with carat between 0 to3 is high in almost every clarity specially 
#in SI2 ,SI1,VS2,Vs1,VVS2 this high density is illustrated better. this density is distributed almost evenly in each price.
#there is visible that in I1 clarity there is only high density in carat between 0 to 3 in prices only from0 to 10000.
#in clarity of VVS1,IF we can see that the density is lower comparing to other 5 previous clarity and from prices higher than 
#15000 , the density starts to decline gradually.
newcarat=log10(carat)
newcarat
newprice=log10(price)
newprice
coplot(newprice~newcarat|clarity,rows=1)
plot(newprice~newcarat,
     xlab="carat",
     ylab="price",
     pch=20,
     cex=1,
     col=factor(clarity,labels=1:8))
legend("bottomright",
       legend = levels(factor(clarity)),
       pch  = 20,
       col  = 1:8)
#as same as the result with coplot, there is high density of SI2 ,SI1,VS2,Vs1,VVS2 in carat higher than -0.2 t0 0.4(with log10)
#each illustrated with distinctive color,as it's shown there is almost an even distribution of density in lower and higher prices
#with this specific clarity, so we can conclude that price is not an effective factor on density of clarity with different carat.
#10(bonus)1
diamonds
m=as.data.frame(table(depth))
m
hist(depth,breaks=100,col=rainbow(10))
#there is obvious that the depth of diamonds can have normal distribution.
coplot(price~depth|cut,rows=1)
#as it is illustrated in the charts, we have 5 cut category and except for the Fair categoyr, there density for depth between 
#55 to 65 in any price is high for any cut category. this means that the better the cut category gets, the depth doesn't change
#and probably the depth in range of 55 to 65 is the ideal for all sorts of cut and price. in Fair category, because it's not
#as good as others, the density has spread around 50 to 70 in depth and there is higher density in 65 to 70 in almost each price.
# it has to be mentioned that price is not the key factor in this chart because depth is not related to it in any cut categor.
#10(bonus)2
plot(price~depth,
          xlab="depth",
          ylab="price",
          pch=20,
          cex=1,
          col=factor(clarity,labels=1:8))
legend("topright",
       legend = levels(factor(clarity)),
       pch  = 20,
       col  = 1:8)
#like the previous chart , this one resulted the same, all the clarity categories,have high density in depth between 55 to 65,
#this range of depth is the most frequent and it has nothing to do with price factor cause the density is evenly distributed,
#regardless of price.
#10(bonus)3
ggplot(data=newdiamonds)+
  geom_point(mapping=aes(x=newprice,y=newcarat,color=cut),size=1,position = 'jitter')
#as we can see, the frequency of ideal cut is overall more than other kind of cut, also the high density of diamonds with carat between 
#0.2 to 0.45 is is stable in almost all price. we can conclude that between carat 0.2 to 0.45, there is more diamonds 
#with cut of "prumium" and " ideal" , the more the price goes up, the more these two categories frequency gets.
#as we go further in x(price) axis,we can see color blue is getting fade. it shows that diamonds with same carat but diffrent cut,
#can have different prices.
#11
table1=table(cut,color)
chi2=chisq.test(table1)
chi2
chi2$expected>5
#all the expected frequencies are greater than 5 so we can say this test is working fine. since the p_value is less than 0.05
#we can conclude that the h0 is not acceptable and so there is a relationship between cut and color.
boxplot(color~cut,
     xlab="cut",
     ylab="color",
     pch=20,
     cex=1,
     col=factor(color,labels=1:7))
legend("topright",
       legend = levels(factor(color)),
       pch  = 25,
       col  = 1:7)
#there is also visible that first 3 cut category are mostly color E and as the category gets better 
#the color changes from I to J.
#12
price1=(price[cut=="Fair"])
price2=(price[cut=="Good"])
price3=(price[cut=="Very Good"])
price4=(price[cut=="Premium"])
price5=(price[cut=="Ideal"])
samp1=runif(5,min(price1),max(price1))
samp2=runif(5,min(price2),max(price2))
samp3=runif(5,min(price3),max(price3))
samp4=runif(5,min(price4),max(price4))
samp5=runif(5,min(price5),max(price5))
table2=stack(data.frame(samp1,samp2,samp3,samp4,samp5))
table2
boxplot(values~ind,data=table2)
#there is shown in the plots that the means of this 5 cuts are not same, now we can do ANOVAL test to make sure of the result.
oneway.test(values~ind,data=table2,var.equal = T)
#p_value is wa greater than 0.05 so we can say that h0 as all the means of samples are equal is acceptable!
anova(lm(values~ind,data=table2))
AN=aov(values~ind,data=table2)
AN
par(mfrow=c(2,2))
plot(AN)
par(mfrow=c(1,1))
#we can see that the hypothesis about variances equality is correct.
#the hypothesis about normal distribution is correct.
kruskal.test(values~ind,data=table2)
#because the p-value is greater than 0.05, we accept the h0 in this test,so all the means are equal. 
TukeyHSD(AN)
plot(TukeyHSD(AN))
#the P adj of all sample's pairs are greater than 0.05 so h1 is not acceptable in any of them , it can be mentioned that non 
#of the means are similar.plus according to the plot, 0 is in all the confidence interval of pairs compared.
#so it is proven the means are equal and there is a relationship between price and cut.
#13
diamonds
newdiamonds=subset(diamonds,carat<0.5)
newdiamonds
par(mfrow=c(2,2))
qqnorm(price[cut=="Fair"])
qqnorm(price[cut=="Good"])
x1=price[cut=="Fair"]
y1=price[cut=="Good"]
#both of them are not normal but have the same distribution shape, so we use Mann-Whitney test.
wilcox.test(x,y,alternative="two.sided")
#p-value is less than 0.05, so we reject the h0 and there is  meaningful difference between the tow means.
#14
p1=sum(price[cut=="Fair"]>1000)/length(price)
p2=sum(price[cut=="Good"]>1000)/length(price)
n=c(length(price[cut=="Fair"]),length(price[cut=="Good"]))
x=c(sum(price[cut=="Fair"]>1000),sum(price[cut=="Good"]>1000))
prop.test(x,n,alternative = "two.sided",correct = F)
#as the result shows, p-value is less than 0.05, so we reject the H0 and we can conclude there is a meaningful 
#difference between p1 and p2.