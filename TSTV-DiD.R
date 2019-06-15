#*** MSBA 6440 ***#
#*** Gordon Burtch ***#
#*** Updated Jan/2019 ***#
#*** Code for Lecture 6 ***#
#*** DiD Regression ***#

rm(list=ls());gc()

library(stargazer)
library(plm)
library(lfe)
library(panelView)
library(ggplot2)

#**** Load the data ***#
MyData<-read.csv("~/Google Drive/Teaching/MSBA 6440/2019/(6) Continuing DiD/TSTV-Obs-Dataset.csv")
qplot(factor(MyData$week))

#*** Difference in Differences Regression ***#
# How does it compare with our old matched estimate of 0.203?
did_basic <- lm(data=MyData,view_time_total_hr~premium*after)
did_log_basic <- lm(data=MyData,log(view_time_total_hr+1)~premium*after)
stargazer(did_basic,did_log_basic,title="DiD Estimates",column.labels=c("Total Viewership", "Log(Total Viewership)"),type="text")

# Let's try replacing the treatment dummy with subject fixed effects.
# What happened to the estimate of premium?
# Note, the plm package can be pretty slow actually, and it includes a lot of functionality we may not use (like First Diffs)
# Try the LFE package's felm command, it's waaaay faster with big datasets.
did_fe <- felm(data=MyData,view_time_total_hr~premium + after + after:premium|id)
did_sfe_tfe <- felm(data=MyData,view_time_total_hr~premium + after + after:premium|id+week)
did_log_sfe_tfe <- felm(data=MyData,log(view_time_total_hr+1)~premium + after + after:premium|id+week)
stargazer(did_fe,did_sfe_tfe,did_log_sfe_tfe,type="text",title="DiD + Time & Subject FEs",column.labels = c("Subject FEs","Subject + Time FEs","Log Subject + Time FEs"))

# Time for our placebo test... 
# Let's limit to pre-period data, and shift the treatment date back in time, artificially, and see if we see sig differences pre treatment.
# Again, recall first week when treatment starts
MyDataPre <- MyData[MyData$after==0,]
max(MyDataPre$week)
MyDataPre$after <- MyDataPre$week > 2224
did_basic_placebo <- lm(data=MyDataPre,view_time_total_hr~premium+after+premium*after)
did_log_basic_placebo <- lm(data=MyDataPre,log(view_time_total_hr+1)~premium+after+premium*after)
stargazer(did_basic_placebo,did_log_basic_placebo,title="DiD Estimates",column.labels=c("Total Viewership", "Log(Total Viewership)"),type="text")

# Note: I probably trust the log DV regression more in this case, given the skewed power distribution in TV viewership hours.
qplot(MyData$view_time_total_hr)

# General indication is that a simple placebo test like this doesn't indicate serious problems with our DiD setup.
# Of course, the choice of the placebo timing is totally arbitrary, which is not good...
# Other placebo's I can run are to use my same data, but replace the DV with some other measure in this context that we known treatment should not influence (no such variable in this case).

# Let's try dynamic DiD instead, as that is "less" arbitrary of an assessment.
# First, relevel the time variable to omit the period just before treatment...
MyData$week <- factor(MyData$week)
levels(MyData$week)
MyData <- within(MyData, week <- relevel(week,ref=7))
levels(MyData$week)

#Now let's run the dynamic model.
did_dyn_sfe_tfe <- felm(data=MyData,view_time_total_hr~premium*week|id+week)
did_dyn_log_sfe_tfe <- felm(data=MyData,log(view_time_total_hr+1)~premium*week|id+week)
stargazer(did_dyn_sfe_tfe,did_dyn_log_sfe_tfe,type="text",title="Dynamic DiD + FEs",column.labels = c("Dynamic DiD + FEs","Dynamic Log DiD + FEs"))

# Let's plot the coefficients and confidence intervals now...
# First, we pull out the coefficients and standard errors from the output object of the regression.
coefs_ses <- cbind(did_dyn_log_sfe_tfe$coefficients,did_dyn_log_sfe_tfe$se)[]
View(coefs_ses)

# We just want to keep the dynamic treatment estimates...
coefs_ses <- coefs_ses[c(15:nrow(coefs_ses)),]
View(coefs_ses)

# We don't get estimates for the reference period so let's plug in 0's for it.
coefs_ses <- data.frame(rbind(coefs_ses[c(1:6),],c(0,0),coefs_ses[c(7:nrow(coefs_ses)),]))
row.names(coefs_ses)[7] <- "premium:week2226"
View(coefs_ses)
row.names(coefs_ses)

# Now let's make it a dataframe, and construct our confidence interval. This is 90% confidence interval. 
coefs_ses <- data.frame(coefs_ses)
names(coefs_ses) <- c("betas","ses")
coefs_ses$ub_90 <- coefs_ses$betas+1.645*coefs_ses$ses
coefs_ses$lb_90 <- coefs_ses$betas-1.645*coefs_ses$ses

# Let's create a time period indicator for the graph that's easy to read. 
coefs_ses$week <- seq(1:nrow(coefs_ses))

# Let's connect the estimates with a line and include a ribbon for the CIs. 
plot <- ggplot(coefs_ses, aes(week,betas)) + geom_ribbon(aes(ymin=lb_90,ymax=ub_90,fill="90% CI"),alpha=0.3) 
plot <- plot + geom_line() + geom_hline(yintercept=0,linetype="dashed",color="gray",size=1) + geom_vline(xintercept=7,linetype="dashed",color="red",size=1) + theme_bw()
plot

# Fitting a smoothed line (lowess curve) to the coefficients, though this is less "rigorous" and more of a heuristic. 
plot <- ggplot(coefs_ses, aes(week,betas))  
plot <- plot + geom_smooth() + geom_hline(yintercept=0,linetype="dashed",color="gray",size=1) + geom_vline(xintercept=7,linetype="dashed",color="red",size=1) + theme_bw()
plot

# Let's try the collapsed version of the Simple DiD to make sure serial correlation isn't a problem.
MyData <- aggregate(MyData,by=list(MyData$id,MyData$after),FUN=mean)
did_collapse <- felm(data=MyData,view_time_tstv_hr~premium:after|id+after)
did_log_collapse <- felm(data=MyData,log(view_time_tstv_hr+1)~premium:after|id+after)
stargazer(did_collapse,did_log_collapse,type="text",title="Collapse DiD + Subject FEs",column.labels = c("Collapse DiD + FEs","Log Collapse DiD + FEs"))
