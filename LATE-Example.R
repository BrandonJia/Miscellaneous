# Author: Gordon Burtch
# Course: MSBA 6440
# Session: Instrumental Variables
# Topic: Local Average Treatment Effect
# Lecture 8

library(AER)
set.seed(1984)

# Let's say our treatment is using emoticons in our email subject lines.
# We expect this to have a causal effect on product purchase because people will be more likely to open / read the
# emails now.
# So, we run an A/B test, where we try using emoticons vs. not when we email people with promotional
# Materials... 
treatment <- rbinom(20000, 1.0, 0.5)

# Treatment causally increases open / read rates.
reademail <- treatment*rbinom(20000,1.0,0.2)

# Purchase is then ultimately influenced by reading. 
# We have baseline purchase, with probability reduced by not opening email, and increased by opening email. 
purchase <- rbinom(20000,1.0,0.1) + reademail*rbinom(20000,1.0,0.05)

summary(lm(purchase~treatment)) #0.010
summary(lm(purchase~reademail)) #0.048
summary(ivreg(purchase~reademail|treatment)) #0.051
summary(lm(reademail~treatment)) # 0.05236




