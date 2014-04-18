## ancova and lm 

##nectar availability 

flower.male<-read.csv("/Users/laurenponisio/Documents/Honor's Thesis/Foraging/Flower counts with males.csv",header=TRUE)

## remember that path name only works on my computer

##for this analysis we are testing whether the nectar availbility in the grazed site is different than the availbility in the ungrazed. Specifically, we are asking whether the slopes of the linear models of nectar availability through time are different between sites. So for this we use an ANCOVA (analysis of covarience) to test for a difference in slopes between two linear models.

##we regress nectar availbility~day*site

lm.tot <- lm(flower.male$f.male~flower.male$day*flower.male$site)

##we use the function anova for perform an ancova (confusing I know)

ancova.nectar<- anova(lm.tot)
ancova.nectar

### the first p value is for the effect of day on nectar availbility, which is significant 
### the second p-value is for the difference in intercepts between sites, which is also significant 
###the third p-value is for the difference in slopes between sites, which is significant as well 

##plotting

#we want to show the difference in slopes between sites, so we create two linear models (one for each site) and then plot them 

plot(flower.male$f.male[flower.male$site=="ug"]~flower.male$day[flower.male$site=="ug"],
	ylim=c(0,1), ## the limits of our y axis
	xlim=c(0,45), ##the limits of our x axis
	ylab="Flowers per male", #the our y axis label
	xlab="Time (days)") ## the x axis label 
	
	
points(flower.male$f.male[flower.male$site=="g"]~flower.male$day[flower.male$site=="g"], pch=16) ##change the pch (point shape) so we can tell ug from grazed. Pch defauts to 1 which is open circles. 16 is closed circles

lm.ug<-lm(flower.male$f.male[flower.male$site=="ug"]~flower.male$day[flower.male$site=="ug"])
abline(lm.ug,lty=2)

lm.g<-lm(flower.male$f.male[flower.male$site=="g"]~flower.male$day[flower.male$site=="g"])
abline(lm.g)

## lets move on to population densities
## we are asking whether the butterfly densities in the grazed areas are different than the densities in the ungrazed. But sadly, we have a time series (the densities of day n+1 are not independent of the densities on day n) and we want to discound the effect of time. We cannot use an ancova to test for a difference in intercept because we have a curve which makes interpreting things difficult. So we can make a combined curve of the densities of both the grazed and ungrazed areas, take the residuals from that "predicted" curve and use a two way anova to ask whether the residuals are different between sites. 

t.d<-read.csv("~/Documents/Honor's Thesis/Population/Final Spreadsheets/Densities.csv",header=TRUE)

## lets fit a second degree polynomial because that is what butterflies densities through time should look like 

t.den.lm.2<-lm(t.d$m.density~t.d$Day+I(t.d$Day^2))

## we creat a new column "resid" by assigning the residuals of our linear model to our data frame 

t.d$resid <- residuals(t.den.lm.2)


## let's check to see if the residuals are normally distributed 

plot(density(t.d$resid))

## looking good. Now for the anova. We are interested in the effect of site and seasonal grazing (the column pre.post for before and after grazing). We create our model using ~ and test for the interaction for site*seasonal grazing on the residuals to the predicted curve 

t.d.anova<-aov(t.d$resid~t.d$site*t.d$pre.post)
summary(t.d.anova)

## nothing is significant =( 
## the first p value refers to the effect site on the residuals
## the second p value refers to the effect seasonal grazed on the residuals
## the third p value refers to the interaction of site and seasonal grazing 

## plotting 

## anova plot 

layout(matrix(1:4, nrow=2)) ## creates a plotting window with two rows and two colums. Check out ?layout 

plot(t.d.anova)

## the first pannel is the fitted values vs. residuals. If your residuals are fitted around the red line (predicted by your model) you are in good shape. 
## the second is the squareroot of the residuals 
##the third is the quantile-quantile plot. It is the quantiles of a normal distribution and the quantiles of your data. If your data (the circles) falls on the line your data is normal and all is well. If not, you should really not be doing an anova
## The forth pannel is cook's distance. Anything beyond the dotted red line is a point that is particularly influencial that drags the model toward it. 


##interaction plot 

interaction.plot(x.factor=t.d$pre.post, trace.factor=t.d$site, response=t.d$resid)

##you have a response which is you dependent variable, and then an x and trace factor which are the interaction terms in your anova. 

## first the residuals 

boxplot(t.d$resid[t.d$site=="ug"], t.d$resid[t.d$site=="g"],
	col=c("darkolivegreen","goldenrod"), ## a vector of colors for the bars
	names= c("Ungrazed", "Grazed"), ## the names of the bars
	xlab="Site",
	ylab= "Residuals")
	
## now density ~ time 

plot(t.d$m.density[t.d$site=="g"]~t.d$Day[t.d$site=="g"],
	pch=16,
	xlim=c(0,45),
	xlab="Day of MRR",
	ylab=expression("Males/"*m^2),
	cex.lab=1.5, ## increase the size of the labels
	cex=2, ## increase the size of the points
	cex.axis=1.5) ## increase the size of the axis

points(t.d$m.density[t.d$site=="ug"]~t.d$Day[t.d$site=="ug"],
	cex=2)

curve(t.den.lm.2$coeff[1] + t.den.lm.2$coeff[2]*x + t.den.lm.2$coeff[3]*x^2,add=TRUE,lty=4)

###let try a t-test just for kicks 

t.test(t.d$resid~t.d$site) ## you can give it y~x or just two things to test for a difference in means

## get get a p value, a t statistic and the degrees of freedom. R clarifies our alternative hypothesis (true difference in means is not equal to 0, our null is true difference in means is equal to 0) and gives us a 95% confidence interval around our difference in means. Notice they over lap zero. We also get the mean for each of our groups.

##non-parametric stats 

## if our data was not normal, we could have used a wilcoxon test instead of a t-test and a kruskal wallis test instead of an anova. We could not have looked for an interaction with KW however. 

?wilcox.test 
?kruskal.test

## contingency table tests 

## my favorite contigency table test is the fisher's exact test because it can be used when sample sizes are small and data are uneuqally distributed amound cells in the table. The marginals (both rows and columns) are also constrained. 

## from ?fisher.test 
##For 2 by 2 tables, the null of conditional independence is equivalent to the hypothesis that the odds ratio equals one. ‘Exact’ inference can be based on observing that in general, given all marginal totals fixed, the first element of the contingency table has a non-central hypergeometric distribution with non-centrality parameter given by the odds ratio (Fisher, 1935).

#######Probability of visitation

## with have two 2x2 tables of the number plants that were visted during an monitoring period and the number that were not (total plant - visited plants), one for before grazed and one for after 

bfly.pre <- matrix(c(55,153-55,22,108-22),ncol=2)

bfly.post <- matrix(c(37,205-37,45,272-45),ncol=2)

fisher.test(bfly.pre)
fisher.test(bfly.post) 

## we get back a p-value, the confidence interval for the odds ratio and an estimate of the odds ratio

## fun with distributions 

pnorm(q=.5, mean=0, sd=1) ## give r a quantile (and the mean and sd) and you get out a p vlaue. qnorm will give you a quantile value from a p value. 

rnorm(n=10,mean=0,sd=1) ## gives you 10 draws from the N(0,1)

rpois(n=10, lambda=10) ## gives your 10 draws from the poisson, mean=variance=lambda distribution 

rbinom(n=10, size=10, prob=0.5) ## 10 draws from a n=10, p=0.5 distirbution. Get out k 

runif(n=10, min=0, max=1) ## every value in between 0 and one has equal probability, all other values prob=0







