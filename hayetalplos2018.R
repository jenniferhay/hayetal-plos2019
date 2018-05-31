#Authors: Hay, Jen, Abby Walker, Kauyumari Sanchez, Kirsty Thompson
#Title: Abstract social categories facilitate access to socially skewed words
#Submitted to PLOS One
#Last updated June 2018

#Code to replicate statistics and figures in paper


library(languageR)
library(lme4)
library(beanplot)
exp1 = read.csv("exp1words.csv")
exp2 = read.csv("exp2words.csv")
exp3 = read.csv("exp3words.csv")
exp4 = read.csv("exp4words.csv")

exp1$Block = as.factor(exp1$Block)
exp2$Block = as.factor(exp2$Block)
exp3$Block = as.factor(exp3$Block)
exp4$Block = as.factor(exp4$Block)

## EXPERIMENT 1

##### EXP 1 ACCURACY

# table 1:
speakermeans = aggregate(ACC ~ WordAge+condition+Subject, data=exp1[exp1$Block != "2",], mean)
means = aggregate(ACC ~ WordAge+condition, data=speakermeans, mean)
sds = aggregate(ACC ~ WordAge+condition, data=speakermeans, sd)

# model reported in text
exp1ACC <- glmer(ACC~  WordAge*cTrial + Block+ (1 + condition*WordAge| Subject) + (1| Stimulus), data = exp1[exp1$Block != "2",], family="binomial",control=glmerControl(optimizer = "bobyqa" ))

#### EXPERIMENT 1 RT
exp1RT <- lmer(RTlog ~  condition*WordAge*older+cTrial + Block+ (1 + condition*WordAge| Subject) + (1 | Stimulus), data = exp1[exp1$Block != "2" & exp1$ACC == 1,]  )

# separate test for young and old (coefs reported in text)
exp1RTyoung <- lmer(RTlog ~  condition*WordAge +cTrial + Block+ (1 + condition*WordAge| Subject) + (1| Stimulus), data = exp1[exp1$Block != "2" & exp1$ACC == 1 & exp1$yearofbirth > 1990,]  )

exp1RTold<- lmer(RTlog ~  condition*WordAge +cTrial + Block+ (1 + condition*WordAge| Subject) + (1| Stimulus), data = exp1[exp1$Block != "2" & exp1$ACC == 1 & exp1$yearofbirth < 1990,]  )

### EXPERIMENT 1 RT PREDICTION FIGURE - YOUNG - FIGURE 3(left)

par(mfcol=c(1,2))

plotLMER.fnc(exp1RTyoung, pred = "WordAge", fun= exp, intr=list("condition",c("youngreal", "oldreal"), "end", list(c("blue", "darkgrey" ), c(1, 2)))  ,  ylabel="RT (ms)", lwd=2,addlines=T, ylim=c(650,750), cex=1.1, xlabel="Word Age", ilabel="") #male gender     

## EXP 1 RT BEANPLOT - YOUNG - FIGURE 3(right)

exp1RTbeandata = exp1[exp1$Block != "2" & exp1$ACC == 1 & exp1$yearofbirth > 1990,]
exp1RTbeandata$WordAge = droplevels(exp1RTbeandata$WordAge)
exp1RTbeandata$condition = droplevels(exp1RTbeandata$condition)
beanplot(RT ~ condition*WordAge, data=exp1RTbeandata, side = "both", xlab="Word Age", ylab="RT (ms)", col = list("grey", "blue"), axes=F, cex=1.2, what=c(0,1,1,0), ylim=c(400,1300))

axis(1, at=c(1, 2),  cex=1.2, labels=c("oldword","youngword"))
axis(2)
legend("bottomleft", fill = c("grey", "blue"),
       legend = c("oldreal", "youngreal"), box.lty=0)


################EXPERIMENT 2 ###################

##### EXP 2 ACC

# table 3
speakermeans = aggregate(ACC ~ WordGender+condition+Subject, data=exp2[exp2$Block != "2",], mean)
means = aggregate(ACC ~ WordGender+condition, data=speakermeans, mean)
sds = aggregate(ACC ~ WordGender+condition, data=speakermeans, sd)

# table 4
exp2ACC <- glmer(ACC ~  Sex+WordGender * condition  + cTrial + (1 + WordGender * condition| Subject) + (1| Stimulus), data = exp2[exp2$Block != "2",]  , family="binomial", control=glmerControl(optimizer = "bobyqa" ))

# plot of interaction (not in manuscript)
# par(mfcol=c(1,1))
# plotLMER.fnc(exp2ACC, pred = "WordGender", fun= plogis, intr=list("condition",c("female-real", "male-real"), "end", list(c("red", "darkblue"), c(1, 2)))  , cex=1.2, lwd=2,addlines=T, xlabel="Word Gender", ylabel="Predicted Accuracy", ilabel="" )


##  EXP 2 RT

# table 5
exp2RT <- lmer(RTlog ~  Sex*condition +Sex*cTrial  + (1 + WordGender*condition| Subject) + (1| Stimulus), data = exp2[exp2$Block != "2" & exp2$ACC == 1,])


### EXP 2 RT Model prediction plot - Figure 5(left)
par(mfrow=c(1,2))
plotLMER.fnc(exp2RT, pred = "condition", fun= exp, intr=list("Sex",c("female", "male"), "beg", list(c("red", "darkblue" ), c(1, 2))), ylabel="RT (ms)", lwd=2,addlines=T, cex=1.1, xlabel="Condition", ilabel="")    

## EXP 2 RT BEANPLOT  - Figure 5(right)

exp2RTbeandata = exp2[exp2$Block != "2" & exp2$ACC == 1,]
exp2RTbeandata$condition = droplevels(exp2RTbeandata$condition)
beanplot(RT ~ Sex*condition, data=exp2RTbeandata, side = "both", xlab="condition", ylab="RT (ms)", col = list("red", "darkblue"), axes=F, cex=1.2, what=c(0,1,1,0), ylim=c(300, 1500))
axis(1, at=c(1, 2),  cex=1.2, labels=c("femalereal","malereal"))
axis(2)
legend("bottomleft", fill = c("red", "darkblue"),
       legend = c("female participant", "male participant"), box.lty=0)

########### EXPERIMENT 3 #########

## EXP 3 ACC 

# table 6
speakermeans = aggregate(ACC ~ WordGender+condition+Subject, data=exp3[exp3$Block != "2",], mean)
means = aggregate(ACC ~ WordGender+condition, data=speakermeans, mean)
sds = aggregate(ACC ~ WordGender+condition, data=speakermeans, sd)

# table 7
exp3ACC <- glmer(ACC ~  WordGender * condition  + cTrial +(1 + condition*WordGender| Subject) + (1| Stimulus), data = exp3[exp3$Block != "2",] , family="binomial", control=glmerControl(optimizer = "bobyqa" ))

# interaction not in manuscript
# par(mfcol=c(1,1))
# plotLMER.fnc(exp3ACC, pred = "WordGender", fun= plogis, intr=list("condition",c("female-real", "male-real"), "end", list(c("gray", "blue"), c(1, 2)))  , cex=1.2, lwd=2,addlines=T, xlabel="Word Gender", ylabel="Predicted Accuracy", ilabel="Pairing" )



## combined EXP 3 and EXP 2 accuracy model
## mentioned in text
exp2$experiment = "two"
exp3$experiment = "three"
exp2and3 = rbind(exp2,exp3)
exp2and3ACC <- glmer(ACC ~  Sex * condition + WordGender*condition + cTrial*Sex*experiment +(1+ condition*WordGender | Subject) + (1| Stimulus), data = exp2and3[exp2and3$Block != "2",], family=binomial, control=glmerControl(optimizer = "bobyqa" ) )

## EXP 3 RT MODEL

# table 8
exp3RT <- lmer(RTlog ~  WordGender*condition + WordGender * cTrial  +(1+ condition * WordGender | Subject) + (1| Stimulus), data = exp3[exp3$Block != "2" & exp3$ACC == 1,])

##  EXP 3 RT Prediction Plot - Figure 7 (left)

par(mfcol=c(1,2))
par(cex=1.5)
plotLMER.fnc(exp3RT, pred = "WordGender", fun=exp, intr=list("condition",c("female-real", "male-real"), "beg", list(c("grey", "blue"), c(1, 2)))  ,cex=1.2, xlabel="Word Gender", ylabel="RT (ms)", ilabel="", lwd=2,addlines=T, ylim=c(670, 700))

# not in manuscript
# par(mfcol=c(1,1))
#plotLMER.fnc(exp3RT, pred = "cTrial", fun=exp, intr=list("WordGender",c("female", "male"), "end", list(c("grey", "blue"), c(1, 2)))  ,cex=1.2, xlabel="Trial (scaled and centered)", ylabel="RT (ms)", ilabel="", lwd=2,addlines=T)

# EXP 3 RT BeanPlot - Figure 7 (right)
exp3RTbeandata = exp3[exp3$Block != "2" & exp3$ACC == 1,]
exp3RTbeandata$WordGender = droplevels(exp3RTbeandata$WordGender)
exp3RTbeandata$condition = droplevels(exp3RTbeandata$condition)
beanplot(RT ~ condition*WordGender, data=exp3RTbeandata, side = "both", xlab="Word Gender", ylab="RT (ms)", col = list("grey", "blue"), axes=F, cex=1.2, what=c(0,1,1,0), ylim=c(300,1300))
axis(1, at=c(1, 2),  cex=1.2, labels=c("female","male"))
axis(2)
legend("bottomleft", fill = c("grey", "blue"),
       legend = c("femalereal", "malereal"), box.lty=0)


#### COMBINED EXP 2 AND 3 RT, mentioned in text

exp2and3RT <- lmer(RTlog ~  WordGender * experiment * cTrial + WordGender * condition *cTrial + Sex*cTrial+Sex*condition  +(1+ condition*WordGender | Subject) + (1| Stimulus), data = exp2and3[exp2and3$ACC == 1  & exp2and3$Block != "2",])

####################  EXPERIMENT 4

##### EXP 4 ACCURACY MODEL

# table 9
speakermeans = aggregate(ACC ~ WordGender+condition+Subject, data=exp4[exp4$Block != "2",], mean)

means = aggregate(ACC ~ WordGender+condition, data=speakermeans, mean)
sds = aggregate(ACC ~ WordGender+condition, data=speakermeans, sd)

# table 10
exp4ACC <- glmer(ACC ~ cTrial * WordGender * condition + condition *Sex*cTrial  + (1 + condition * WordGender | Subject) + (1| Stimulus), data = exp4[exp4$Block != "2",] , family="binomial", control=glmerControl(optimizer = "bobyqa" ) )



#### EXP 4 ACCURACY PREDICTION PLOTS

#Figure 9 (left and right)

par(mfcol=c(1,2))
par(cex=1.2)
# not in manuscript
# plotLMER.fnc(exp4ACC, pred="cTrial", fun=plogis,
#              intr=list("WordGender", c("female", "male"), "end", list(c("grey", "blue"), c(1, 2))), ylim=c(.87, .99),
#              control=list("conditionmale-real", 0), cex=1, lwd = 2, xlabel="Trial (scaled and centred)", ylabel="Predicted Accuracy", ilabel="")
# title("female-real")
# 
# plotLMER.fnc(exp4ACC, pred="cTrial", fun=plogis,
#              intr=list("WordGender", c("female", "male"), "beg", list(c("grey", "blue"), c(1, 2))),
#              control=list("conditionmale-real", 1), cex=1, lwd=2, xlabel="Trial (scaled and centred)", ylim=c(.87, .99),ylabel="Predicted Accuracy", ilabel="")
# title("male-real")

plotLMER.fnc(exp4ACC, pred = "cTrial", fun=plogis, intr=list("condition",c("female-real", "male-real"), "end", list(c("grey", "blue"), c(1, 2)))  ,cex=1.2, xlabel="Trial", ylabel="Accuracy", ilabel="", lwd=2,addlines=T, control=list("WordGendermale", 0))
title("Female Words")
plotLMER.fnc(exp4ACC, pred = "cTrial", fun=plogis, intr=list("condition",c("female-real", "male-real"), "end", list(c("grey", "blue"), c(1, 2)))  ,cex=1.2, xlabel="Trial", ylabel="Accuracy", ilabel="", lwd=2,addlines=T, control=list("WordGendermale", 1))
title("Male Words")

#Figure 10 (left and right)

par(mfcol=c(1,2))
plotLMER.fnc(exp4ACC, pred="cTrial", fun=plogis,
             intr=list("Sex", c("female", "male"), "end", list(c("red", "darkblue"), c(1, 2))),
             control=list("conditionmale-real", 0), cex=1, xlabel="Trial (scaled and centred)", ylim=c(.87, .99), ylabel="Predicted Accuracy", lwd=2, ilabel="Participant")
title("female-real condition")
plotLMER.fnc(exp4ACC, pred="cTrial", fun=plogis,
             intr=list("Sex", c("female", "male"), "end", list(c("red", "darkblue"), c(1, 2))),
             control=list("conditionmale-real", 1), cex=1, xlabel="Trial (scaled and centred)", ylim=c(.87, .99),ylabel="Predicted Accuracy", lwd = 2, ilabel="Participant")
title("male-real condition")



###### EXP 4 RT

# table 11

exp4RT<- lmer(RTlog ~ Block+cTrial * WordGender * condition + (1 +  condition * WordGender | Subject) + (1 |      Stimulus), data = exp4[exp4$Block !="2" &  exp4$ACC == 1,])



# fig 11

par(mfcol=c(1,2))
par(cex=1.2)
plotLMER.fnc(exp4RT, pred = "cTrial", fun=exp, intr=list("condition",c("female-real", "male-real"), "beg", list(c("grey", "blue"), c(1, 2)))  ,cex=1.2, xlabel="Trial", ylabel="RT (ms)", ilabel="", lwd=2,addlines=T, control=list("WordGendermale", 0), ylim=c(630,720))
title("Female Words")
plotLMER.fnc(exp4RT, pred = "cTrial", fun=exp, intr=list("condition",c("female-real", "male-real"), "end", list(c("grey", "blue"), c(1, 2)))  ,cex=1.2, xlabel="Trial", ylabel="RT (ms)", ilabel="", lwd=2,addlines=T, control=list("WordGendermale", 1), ylim=c(630,720))
title("Male Words")
