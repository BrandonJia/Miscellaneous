# Author: Gordon Burtch
# Course: MSBA 6440
# Session: Fixed Effects
# Topic: Simulating Fixed Effects
# Lecture 5

library(splitstackshape)
library(stargazer)
library(plm)
library(lfe)

set.seed(1001)

# We begin by simulating our dataset... use the same seed to get the same results as me.

# Let's build our panel, such that we have a fixed subject ID and static feature (weight)
weight <- rnorm(500, mean = 180, sd=30)
id <- factor(seq(1:500))
df <- data.frame(cbind(id,weight))
df <- expandRows(df, count=4,count.is.col = FALSE)
df$period <- rep(seq.int(nrow(df)/4),4)
df$period <- (df$period-1)%%4
df <- df[order(df$id,df$period),]
df$Treat <- round(df$weight/max(df$weight)+runif(2000,0,1)-1,0)

# Let's make sure our treatment variable is correlated with the confounder now.
cor(df$Treat,df$weight)

# Finally, let's simulate the outcome.
df$Y <- 0.6*df$Treat + 0.3*df$weight+rnorm(nrow(df),mean=0,sd=1) 

# Let's look at the data to make sure it isn't wonky...
View(df)

# Okay, so let's start off by looking at the omitted variable bias again.
correct_reg <- lm(df$Y~df$Treat+df$weight)
omitted_reg <- lm(df$Y~df$Treat)
stargazer(correct_reg,omitted_reg,title="True vs. Omitted Regression",column.labels = c("True","Omitted"), type="text")
     
# Now... let's take advantage of our panel data and try a fixed effect regression.   
within_reg <- plm(data=df,Y~Treat,index=c("id"),effect="individual",model="within")
within_reg2 <- felm(data=df, Y~Treat | id)
stargazer(correct_reg,omitted_reg,within_reg,within_reg2,title="True vs. Omitted vs. Within",column.labels = c("True","Omitted","Within","Within 2"), type="text")

# Finally, let's look at First Differences...
fd_reg <- plm(data=df,Y~Treat,index=c("id"),effect="individual",model="fd")
stargazer(correct_reg,omitted_reg,within_reg,fd_reg,title="True vs. Omitted vs. Within vs. First Diff",column.labels = c("True","Omitted","Within","First Diff"), type="text")

# Annnd... let's show the equivalence of Within and LSDV... 
lsdv_reg <- lm(data=df,Y~Treat+factor(id))
stargazer(correct_reg,omitted_reg,within_reg,lsdv_reg,title="True vs. Omitted vs. Within vs. LSDV",column.labels = c("True","Omitted","Within","LSDV"), type="text",omit="id")

