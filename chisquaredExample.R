#######################
## Create data frame ##
#######################

df<-data.frame(
  smoker=c(rep("neverSmoker",38),rep("pastSmoker",23),rep("currentSmoker",21),rep("neverSmoker",164),rep("pastSmoker",17),rep("currentSmoker",19)),
  lungCancer=c(rep("noLungCancer",200),rep("lungCancer",82)),
  stringsAsFactors=F
)
df$smoker<-factor(df$smoker,levels=c("neverSmoker","pastSmoker","currentSmoker"))
df$lungCancer<-factor(df$lungCancer,levels=c("noLungCancer","lungCancer"))

observed<-table(df$lungCancer,df$smoker)

print(observed)


######################################
## Compute chi-squared -- 'by hand' ##
######################################

expected<-observed
N<-sum(observed)

for(i in 1:nrow(observed)){
  for(j in 1:ncol(observed)){
    expected[i,j]<-N*(sum(observed[i,])/N)*(sum(observed[,j])/N)
  }
}

ChiSq.manual<-sum((observed-expected)^2/expected)

print(ChiSq)


############################################################
## Compute chi-squared -- using the chisq.test() function ##
############################################################

ChiSq.direct<-chisq.test(observed)$statistic

print(ChiSq.direct)

## Note that the chisq.test() function also computed the degrees of freedom and p-value for the chi-squared statistic
chisq.test(observed)

