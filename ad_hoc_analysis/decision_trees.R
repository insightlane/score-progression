install.packages("randomForest")
library(randomForest)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

booktimesdt <- booktimes %>%
        ungroup() %>%
        filter(Event != "RB") %>%
        mutate(GoalFlag = ifelse(Event == "G", 1, 0))

fit <- rpart(GoalFlag ~ QtrTimePerc + TimePerc + PreAbsMargin + PreScorerMargin +  ScorerFinalMargin,
             method="poisson", data=booktimesdt)

printcp(fit)

plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(fit, col="blue", type=3, extra=2, main="Classification Tree for Kyphosis Data with number in each node.")

rpart(Score ~ QtrTimePerc + TimePerc + PreAbsMargin + PreScorerMargin + ScorerFinalMargin,
             method="anova", data=booktimes)

plot(fit2, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)

install.packages("party")
library(party)


partytree <- ctree(Score ~ QtrTimePerc + TimePerc + PreAbsMargin + PreScorerMargin, 
            booktimes, controls = ctree_control(maxsurrogate = 3, maxdepth = 2))
plot(partytree)