#==== SETUP====
library(lme4)

#Coding matrix for response trees
mat <-cbind (n1 = c(0,1,1,1,1,1), 
             n2 = c(NA, NA,0,1,1,1),
             n3 = c(NA, NA, NA, NA, 0, 1))

#Now we rename responses in matrix so we can make sense of outcomes
rownames(mat) <- c('no approach', 'approach', 'no subjugate', 'subjugate','no consume','consume')

#Check the matrix looks good, named correctly
mat

#Read in datasets
data.katy <-read.csv("mag response tree BOTH EXPOSURE.csv")
View(data.katy)

data.crick <-read.csv("cricket-response-both.csv")
View(data.crick)
ball1<-read.csv("ball-response-both.csv")


#==== KATYDID ANALYSIS ====

#Binding data to matrix

katy.mat<-(data.katy[rep(1:nrow(data.katy),each=ncol(mat)),]) #breaks each observation down into an entry for each node (col) in the matrix 
katy.mat$observ<-rep(paste('obs',c(1:nrow(data.katy)),sep=''),each=ncol(mat)) #this labels each observation to group them
katy.mat$node<-rep(paste('n',c(1:ncol(mat)),sep=''),nrow(data.katy)) #now each node is coupled to the rows per observation
katy.mat$value<-mat[cbind(as.character(katy.mat$response),katy.mat$node)] #node scores are coded according to the response which match the row names on the matrix
View(katy.mat)

#GLM of katydid
#setting '0+' means the intercept is the halfway point between 1 or 0 for any node, instead of setting it to one option
#node checks value by node, and node:treatment is the variation at node according to treatment
#exposure looks at variation in value according to exposure overall
#node:exposure examines influecne of exposure on each node/between nodes
#(1|individual) and (1|observ) set individual bird and observation (each bird was observed twice) as fixed effects
model.katy <- glmer(value ~ 0+node+node:treatment+exposure+node:exposure+(1|individual)+(1|observ),
                    family=binomial(link=logit),
                    na.action=na.omit,data=katy.mat,
                    control=glmerControl(optimizer="bobyqa"))
summary(model.katy)

#singularity warning is because of node1:node2 and node1:ndd3.exposure
#These are close to 1 because almost all birds aproached, and almost all birds who made it to node3 on the second exposure ate the katydid
#so there is no need to heed the warning or further transform the data and the model is upheld

#calculating likelihoods
#let's have a look at the Probability for the consumption (node 3)
#first we need intercept values
logit_n3_control<-fixef(model.katy)['noden3']
logit_n3_control #=-0.3005915  

logit_n3_eff<-fixef(model.katy)['noden3:treatmentnaive']
logit_n3_eff #=-2.127634 

#probability
p_n3_control<-1/(1+exp(-logit_n3_control))
p_n3_control #0.4254129 

p_n3_naive<-1/(1+exp(-logit_n3_control-logit_n3_eff))
p_n3_naive #=0.08104556

0.4254129/0.08104556  #=5.249059
#so FAMILIAR birds are 5 times more likely to consume after subjugation than naive ones/naive are 5x less likely to consume across all encounters


#now likelihood for consumption at second exposure compared to first
logit_n3_control<-fixef(model.katy)['noden3']
logit_n3_control #=-0.3005915  this stays the asme because we need overall probability of consumption

logit_e2n3_eff<-fixef(model.katy)['noden3:exposuresecond']
logit_e2n3_eff #=2.286332

#probability
p_n3_control<-1/(1+exp(-logit_n3_control))
p_n3_control #0.4254129 

p_n3_e2<-1/(1+exp(-logit_n3_control-logit_e2n3_eff))
p_n3_e2 #=0.8792918

0.8792918/0.4254129  #=2.066914
#birds in the second exposure are twice as likely to consume a katydid if presented than overall

#====CRICKET RESPONSE TREE====

data.crick <-read.csv("cricket-response-both.csv")
View(data.crick)

#binding data to the same matrix
cricket.mat<-(data.crick[rep(1:nrow(data.crick),each=ncol(mat)),])
cricket.mat$observ<-rep(paste('obs',c(1:nrow(data.crick)),sep=''),each=ncol(mat))
cricket.mat$node<-rep(paste('n',c(1:ncol(mat)),sep=''),nrow(data.crick))
cricket.mat$value<-mat[cbind(as.character(cricket.mat$response),cricket.mat$node)]
View(cricket.mat)

#correcting a misread column name
head(cricket.mat)
names(cricket.mat)[names(cricket.mat) == "ï..individual"] <- "individual"


#the model conditions for the cricket are the same as we have enough to do both exposures
model.cricket <- glmer(value ~ 0+node+node:treatment+exposure+node:exposure+(1|individual),
                  family=binomial(link=logit),
                  na.action=na.omit,data=cricket.mat,
                  control=glmerControl(optimizer="bobyqa"))
summary(model.cricket)

#the warning messages are in relation to the fact that almost every bird when presented with a cricket will eat it, so there are very few counts of failes to approach/subjugate, and other options are very rarely ever achieved eg. no subjugation 
#this is another way of saying the behaviour is very very consistent
#so it means that the stand. err might be less accurate

#We can drop (1|obs) because there was no variance and the model fits a little better

#==== BALL====
#Firstly, there were very low numbers of repeated interactions so we cannot run exposure on the entire dataset. We instead have to subset it and test each exposure seperately. 
#there is simply not enough power in the data for us to analyse them together

ball1<-read.csv("ball-response-both.csv")

names(ball1)[names(ball1) == "ï..individual"] <- "individual"
head(ball1)


#Subsetting 

ball.first<-subset(ball1, ball1$exposure=="first")
ball.second<-subset(ball1, ball1$exposure=="second")

#First exposure
ballF.mat<-(ball.first[rep(1:nrow(ball.first),each=ncol(mat)),])
ballF.mat$observ<-rep(paste('obs',c(1:nrow(ball.first)),sep=''),each=ncol(mat))
ballF.mat$node<-rep(paste('n',c(1:ncol(mat)),sep=''),nrow(ball.first))
ballF.mat$value<-mat[cbind(as.character(ballF.mat$response),ballF.mat$node)]
View(ballF.mat)

model.ball1<- glmer(value ~ 0+node+node:treatment+(1|individual),
                   family=binomial(link=logit),
                   na.action=na.omit,data=ballF.mat,
                   control=glmerControl(optimizer="bobyqa"))

summary(model.ball1)
#The warning is because of lack of variation in responses, it means that the standard error might not be as accurate as it could be 

#second exposure
ballS.mat<-(ball.second[rep(1:nrow(ball.second),each=ncol(mat)),])
ballS.mat$observ<-rep(paste('obs',c(1:nrow(ball.second)),sep=''),each=ncol(mat))
ballS.mat$node<-rep(paste('n',c(1:ncol(mat)),sep=''),nrow(ball.second))
ballS.mat$value<-mat[cbind(as.character(ballS.mat$response),ballS.mat$node)]
View(ballS.mat)

model.ball2<- glmer(value ~ 0+node+node:treatment+(1|individual),
                    family=binomial(link=logit),
                    na.action=na.omit,data=ballS.mat,
                    control=glmerControl(optimizer="bobyqa"))

summary(model.ball2)
