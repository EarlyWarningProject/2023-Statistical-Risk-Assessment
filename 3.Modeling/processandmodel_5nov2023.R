# Code to run main models
# Now amended to do lots of investigation and comparison stuff.
# This version manipulated the omitted region fixed effect for 
# compatability with downstream interpretation scripts.

library(purrr)
library(dplyr)
library(glmnet)
library(ggplot2)
library(pROC)
setwd("/Users/nmbryce/Documents/R/SRA/2023-Statistical-Risk-Assessment-github/3.Modeling")
getwd()

savenameaffix = "9Nov_hackregion"
# Function to use at estimation time ====
elogit.fit <- function(ytrain, Xtrain, Xtest, alpha = .5, usepredictors=NULL){
    
  #Don't deal with missingness -- assume it has already been dealt with
  #to force user to recognize it on the data side.
  xtrain = as.matrix(subset(Xtrain, select = usepredictors))
  xtest = as.matrix(subset(Xtest, select = usepredictors))
  set.seed(90035)
  elastic.cv <- cv.glmnet(y=ytrain, x=xtrain, alpha=alpha, family="binomial")
  coeffs <- coef(elastic.cv, s = "lambda.min")
  elastic.predictions = predict(elastic.cv,newx=xtest, s="lambda.min", type="response")
  
  risk <- as.numeric(elastic.predictions)
  #out <- list(risk, coeffs, elastic.cv)
  out = list()
  out$risk = risk
  out$coeffs = coeffs
  out$elastic.cv = elastic.cv
  return(out)
}

# Read in data ====
#alldata = read.csv("../Make-new-base-data/output/prepared2022predictors-2023-09-24.csv")
alldata = read.csv("prepared2022predictors-2023-10-26-2.csv")

# Additional data processing ====
# This will likely be moved into earlier data preparation scripts, 
# but for workflow purposes, it here now.

# The "includenonstate" variable, just an indicator for being
# 1989 or later, got messed up. Correct.

alldata <- alldata %>% mutate(includesnonstate = ifelse(year>=1989,1,0))

# Add in a new country age, countryage_new.
alldata = alldata %>%
  group_by(country) %>%
  mutate(min_year = min(year, na.rm = TRUE), # Find the minimum year for each country
         countryage_new = year - min_year) %>% # Calculate country age
  mutate(countryage_new_ln = log2(countryage_new+1)) %>%
  ungroup() 

# Get mean/probability of mkonset in that country up to that point in time.
alldata <- alldata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    cumulative_sum = cumsum(mk_onset),
    mean_mk_onset = ifelse(countryage_new > 1, cumulative_sum / countryage_new, 0)
  ) %>%
  ungroup()

# Print the data to see the new variable
#print(alldata)

# Let's also create a "new country" indicator that stays on for 2 years
# Just 2 based on some separate tests (maximized the Fstat in a bivariate reg)
alldata = alldata %>% mutate(newcountry = if_else(countryage_new<=2,1,0))

# Let's also create lag of year and year.sq for modeling a time trend; the lag
# is to use at prediction time so we aren't extrapolating.
alldata <- alldata %>%
  group_by(country) %>%
  mutate(year_l1 = lag(year, n = 1, order_by = year),
         year_sq_l1 = year_l1^2)

alldata <- alldata %>% mutate(year_sq = year^2)

### World Bank new data inputs
#- wdi.popsize.new and wdi.popsize.ln (I didn't rename these because I did this earlier today when this was the only change)
#- wdi.trade.ln.new.2
#- imr.sqrt.new.2 (this is the same as imr.sqrt.new but with the newly merged in WB data instead of starting with our prior data; also includes Mitchell and gapminder data)
#- wdi.gdppcgrow.new.2
#- wdi.gdppc.new.2 (not in the model but created in case we need it for comparison)


# Modeling for infant mortality business.
# First we will do "only forward modeling" to avoid retrospective influence.
# (i.e. if an MK affects infant mortality, we don't want to use future
# values to affect past predictions of infant mortality).

alldata$imr.pure = alldata$imr.sqrt.new.2^2

alldata <- alldata %>% 
  arrange(country, year) %>%   # Make sure to arrange by country as well to get the lag within each country
  group_by(country) %>%        # Group by country if you want the lag to reset for each country
  mutate(im.l1 = lag(imr.pure, order_by = year),
         popsize.l1 = lag(wdi.popsize.new, order_by = year),
         gdppcgrowth.l1 = lag(wdi.gdppcgrow.new.2, order_by = year),
         wdi.trade.ln.new.2.l1 = lag(wdi.trade.ln.new.2, order_by=year))

# View the missingness of tradeshare when preceeded by non-missing
alldata %>% filter(!is.na(wdi.trade.ln.new.2.l1) & is.na(wdi.trade.ln.new.2)) %>% 
  select(country, year, wdi.trade.ln.new.2.l1, wdi.trade.ln.new.2) %>% 
  View()

# plot missingness by year
missingness_by_year_tradeshare = alldata %>% group_by(year) %>% summarise(mean_missingness=mean(is.na(wdi.trade.ln.new.2)))
ggplot(missingness_by_year_tradeshare, aes(x = year, y = mean_missingness)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", 
       y = "Mean Missingness", 
       title = "Mean Missingness of wdi.trade.ln.new.2 by Year") +
  theme_minimal()

# There is not a ton of internal missingness, but there is a good bit of overall 
# missingness, even post 1960.

alldata %>% filter(year>=1960 & is.na(wdi.trade.ln.new.2)) %>% nrow() 

# How about the old variable:
alldata %>% filter(year>=1960 & is.na(tradeshare.ln.combined)) %>% nrow() 
# ... so good.

# How much jumping around does one see in the old tradeshare variable?
tradesharespaghetti <- ggplot(alldata, aes(x = year, y = tradeshare.ln.combined, 
  color = as.factor(country))) + geom_line(aes(group = country), size = 0.5) +
  labs(
    title = "Spaghetti plot of tradeshare.ln.combined",
    x = "Year",
    y = "tradeshare.ln.combined",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d()  # Optional: Change color scale

# Display the plot
print(tradesharespaghetti)
# Save the plot to a PDF
ggsave("tradesharespaghetti.pdf", plot = tradesharespaghetti, width = 10, height = 7, units = "in")

# We will stick with the old variable. But change over to log2:

alldata = alldata %>% mutate(tradeshare.log2.combined = log2(exp(tradeshare.ln.combined)))

# View the missingness of IMR when preceeded by non-missing
alldata %>% filter(!is.na(im.l1) & is.na(imr.pure)) %>% 
  select(country, year, im.l1, imr.pure) %>% 
  View()

# The above View() confirms that the only missingness that is preceeded by non-missingness is in 2022.
# So we can just model on what's there and predict where there is missingness

alldata$imr.fwd.fill = NA
alldata$imr.all.fill = NA

countrylist=unique(alldata$country)

for (j in 1:length(countrylist)){
  thiscountry=countrylist[j]
  print(thiscountry)
  thiscountrydata = alldata %>% filter(country==thiscountry)
  
  try({
    lm.out <- lm(log(imr.pure) ~ poly(year, 2, raw = TRUE), data = thiscountrydata)
    
    # fill just going forward to 2022
    lm.predict.2022 = predict(lm.out, newdata=data.frame(year=2022))
    #alldata[alldata$country==thiscountry & alldata$year==2022, "imr.fwd.fill"]= exp(lm.predict.2022)
    imr.vector = thiscountrydata$imr.pure
    imr.vector[thiscountrydata$year==2022]= exp(lm.predict.2022)
    alldata[alldata$country==thiscountry,"imr.fwd.fill"] = imr.vector
    
    # fill any missing, even going backwards in time
    lm.predict.all = predict(lm.out, newdata=thiscountrydata)
    imr.predicted.all = exp(lm.predict.all)
    imr.vector = thiscountrydata$imr.pure
    whichmissing=which(is.na(imr.vector))
    imr.vector[whichmissing]=imr.predicted.all[whichmissing]
    alldata[alldata$country==thiscountry,"imr.all.fill"] = imr.vector
  }, silent = TRUE)

}


alldata  = alldata %>% 
  mutate(imr.all.fill.sqrt = sqrt(imr.all.fill),
         imr.fwd.fill.sqrt = sqrt(imr.fwd.fill))

alldata %>% select(country, year, imr.pure, imr.all.fill.sqrt, 
                   imr.fwd.fill.sqrt) %>% View()


# plot missingness by year
missingness_by_year = alldata %>% group_by(year) %>% summarise(mean_missingness=mean(is.na(imr.fwd.fill)))
ggplot(missingness_by_year, aes(x = year, y = mean_missingness)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", 
       y = "Mean Missingness", 
       title = "Mean Missingness of imr.fwd.fill by Year") +
  theme_minimal()

# ALL THE ABOVE imr business would need to change if 
# we later have IMR data with "internal missingness"


# Similar process for population:
alldata %>% filter(!is.na(popsize.l1) & is.na(wdi.popsize.new)) %>% 
  select(country, year, popsize.l1, wdi.popsize.new) %>% 
  View()

# No 2022 or other internal missingness on wdi.popsize.new!

alldata = alldata %>% mutate(wdi.popsize.log2 = log2(wdi.popsize.new))

### DEAL WITH GDP growth HERE NEXT
# View the missingness of gdppcgrowth 
alldata %>% filter(is.na(wdi.gdppcgrow.new.2), year>=1980) %>% 
  select(country, year, gdppcgrowth.l1, wdi.gdppcgrow.new.2, wdi.gdppc.new, gdppcgrowth, gdppcgrowth.combined) %>% 
  View()

# Too much internal missingness. We won't use.  Stick with gdppcgrowth.combined.



# Partyban (new) should be a factor; we could use factor() and then model.matrix
# to drop it into glmnet, but I'd rather handle it explicitly.
# Now done prior to importing data.

# alldata <- alldata %>%
#   mutate(
#     partyban.new.0 = if_else(is.na(partyban.new), NA_real_, if_else(partyban.new == 0, 1, 0)),
#     partyban.new.1 = if_else(is.na(partyban.new), NA_real_, if_else(partyban.new == 1, 1, 0)),
#     partyban.new.2 = if_else(is.na(partyban.new), NA_real_, if_else(partyban.new == 2, 1, 0))
#   )

# The DVs -- leaded onsets -- need to be corrected. (There are variables called mk.onset.1 and .2 in the data
# but they appear to be lags rather than leads!)

alldata <- alldata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(mk_onset_1 = lead(mk_onset, order_by = year)) %>%
  mutate(mk_onset_2 = lead(mk_onset, n = 2, order_by = year)) %>% 
  mutate(mk_onset_prev_year = lag(mk_onset)) %>% 
  ungroup()  # Remove grouping
    

# Create the "1-2 year window in a way that doesn't go NA easily. 
alldata$mk_onset_1or2 = 0
alldata$mk_onset_1or2[alldata$mk_onset_1==1] = 1
alldata$mk_onset_1or2[alldata$mk_onset_2==1] = 1

# Remove the incorrect ones:
alldata <- alldata %>%
  select(-mk.onset.1, -mk.onset.2, -mk.onset.3, -mk.onset.2window, -mk.onset.3window)

# Create a new mkever variable that =1 when either responsible_ever or 
# location_ever =1 

alldata <- alldata %>% 
  mutate(mk_ever = ifelse(mk_responsible_ever==1 | mk_location_ever==1,1,0))

# Create a logged mk_ongoing_count. Use log2 for interpretability.
alldata <- alldata %>%
  mutate(mk_ongoing_count_log = log2(mk_ongoing_count+1))


alldata %>% 
  group_by(year) %>%    
  summarise(n_missing = sum(is.na(.data$imr.lag.2))) %>% View()

# Shift to using log-base-2 for popsize.ln.combined and remove the old
alldata = alldata %>% mutate(popsize.ln2.combined=log2(exp(popsize.ln.combined)))
alldata = alldata %>% select(-popsize.ln.combined)

# Same for battledeaths
alldata = alldata %>% mutate(battledeaths.ln2=log2(exp(battledeaths.ln)))
alldata = alldata %>% select(-battledeaths.ln)

                             
# Check:
# alldata %>% filter(country=="Sudan") %>% 
#  select(year, mk_onset, mk_onset_1, mk_onset_2, mk_onset_1or2, mk.onset.1, mk.onset.2) %>% View()

# FINALLY, TEMPORARILY REMOVE PRE 1960
alldata_allyears = alldata
alldata = alldata %>% filter(year>=1960)

# Request made to use reg.afr as the reference group, 
# which necessitates adding a reg.na for north america.
alldata$reg.na = 1 - alldata$reg.afr - alldata$reg.eap - alldata$reg.eur - alldata$reg.mna - alldata$reg.sca

table(alldata$reg.na) # make sure its only 0 or 1.

# Save resulting data:
write.csv(file=paste0("usedata_",savenameaffix,".csv"),alldata)


# Define models ====

outcomenames = c("mk_onset","mk_onset_1","mk_onset_2", "mk_onset_1or2")

# Model "1"
# 20 Sept 2023 -- dropping women in parliament due to data concerns.
# -- dropping FOM due to concerns: "v2cldmovem_reg", "v2cldmovew_reg","v2cldmovem_avg","v2cldmovew_avg", 
#"freemove_men4","freemove_women4"
# Use mk_ever rather than location and responsible ever.
# Use mk_ongoing_count_log instead of binary mk_ongoing

# Use imr.sqrt, not imr.lag.2, due to missingness problem 
# with a lagged variable in the beginning years of any new country. 


predictornames.fullest <- c("mean_mk_onset","mk_onset_prev_year", "year", "year_sq",
                      "mk_ever", "mk_ongoing_count_log", "newcountry",
                     "widetargeting", "narrowtargeting",
                     "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", 
                     "countryage_new_ln", "wdi.popsize.log2",
                     "gdppcgrowth.combined", "ios.iccpr1","includesnonstate",
                     "minorityrule", "battledeaths.ln2","judicialreform",
                     "religiousfreedom", "pol_killing_approved","freediscussion",
                     "social_inequality","even_civilrights","repress_civilsoc",
                     "social_power_dist", "ses_power_dist","tradeshare.log2.combined",
                     "coup.try.5yr", "efindex", "discrimpop",
                     "partyban.new.0","partyban.new.1", "partyban.new.2",
                     "v2csgender_binary","v2mecenefm_binary", "imr.fwd.fill.sqrt")

# Check:  
setdiff(predictornames.fullest, names(alldata))

# Alternative models for testing
# Right now this uses the IMR modeled forward in time only to fill in missingness.
predictornames.alt = setdiff(predictornames.fullest, c("imr.fwd.fill.sqrt"))
predictornames.alt = c(predictornames.alt,"imr.all.fill.sqrt")

# One other model for testing.
predictornames.noprev = setdiff(predictornames.fullest, "mk_onset_prev_year")

# 
# # Model 3: The old model, though on somewhat new data 
# predictornames.old = c("mk_ongoing","mk_ever", 
#                       "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", 
#                       "countryage_new_ln", "popsize.ln2.combined", "imr.sqrt", 
#                       "gdppcgrowth.combined", "ios.iccpr1","includesnonstate",
#                       "minorityrule", "elf.ethnic", "battledeaths.ln2",
#                       "candidaterestriction", "partyban","judicialreform",
#                       "religiousfreedom", "pol_killing_approved",
#                       "freemove_men4","freemove_women4", "freediscussion",
#                       "social_inequality","even_civilrights","repress_civilsoc",
#                       "social_power_dist", "ses_power_dist","tradeshare.ln.combined",
#                       "coup.try.5yr")
# 
# setdiff(predictornames.old, names(alldata))

# Just to review:
# What's in the old variables but not new?
#setdiff(predictornames.old, predictornames.fullest)
# What's in new and not the old?
#setdiff(predictornames.fullest, predictornames.old)


# Non-prediction variables worth keeping around
extravariables = c("country","ccode","year","year_l1","year_sq_l1","reg.na")

#-----------------------------------------------------#
# Run models ==== 
# Do it in a tedious way, repeating code for each model
# to keep separate records 
#-----------------------------------------------------#

### Model 1: full ====

# Construct training and test data, pausing to look at missingness.
traindata.full = alldata %>% filter(year<=2020) %>% 
  select(all_of(c(predictornames.fullest, outcomenames, extravariables)))
nrow(traindata.full)
table(traindata.full$year)

#now remove missing to compare
traindata.full = traindata.full %>% filter(complete.cases(.))
nrow(traindata.full)
table(traindata.full$year)

# Repeat for 2021's forecasts
# Construct training and test data, pausing to look at missingness.
traindata.full.2021 = alldata %>% filter(year<=2019) %>% 
  select(all_of(c(predictornames.fullest, outcomenames, extravariables)))
nrow(traindata.full.2021)
table(traindata.full.2021$year)

#now remove missing to compare
traindata.full.2021 = traindata.full.2021 %>% filter(complete.cases(.))
nrow(traindata.full.2021)
table(traindata.full.2021$year)

# Test data:
testdata.full = alldata %>% filter(year==2022) %>% 
                      select(all_of(c(predictornames.fullest, extravariables)))

testdata.full.2021 = alldata %>% filter(year==2021) %>% 
  select(all_of(c(predictornames.fullest, extravariables)))

# Use the lagged years in the test so we aren't extrapolating
# We'll turn this off...
#testdata.full$year = testdata.full$year_l1
#testdata.full$year_sq = testdata.full$year_sq_l1

nrow(testdata.full)

testdata.full = testdata.full %>% filter(complete.cases(.))
nrow(testdata.full)

testdata.full.2021 = testdata.full.2021 %>% filter(complete.cases(.))
nrow(testdata.full.2021)


model.full.out = elogit.fit(ytrain=traindata.full$mk_onset_1or2, 
                  Xtrain = traindata.full,
                  Xtest = testdata.full,
                  alpha = .5,
                  usepredictors = predictornames.fullest)

usecoefs = model.full.out$coeffs
subtract.off =usecoefs["reg.afr",] 
coefnames = rownames(usecoefs)
coefnames[coefnames=="reg.afr"]="reg.na"

rownames(usecoefs)=coefnames

usecoefs["reg.na",] = 0 - subtract.off
usecoefs["reg.eap",] = usecoefs["reg.eap",] - subtract.off
usecoefs["reg.eur",] = usecoefs["reg.eur",] - subtract.off
usecoefs["reg.mna",] = usecoefs["reg.mna",] - subtract.off
usecoefs["reg.sca",] =  usecoefs["reg.sca",] - subtract.off

usecoefs["(Intercept)",] = usecoefs["(Intercept)",] + subtract.off

saveRDS(usecoefs, file = paste0("usecoefs_",savenameaffix,".rds"))

# Save the cv.glmnet object to a file
#saveRDS(model.full.out$elastic.cv, file = paste0("main_full_cv_glmnet_object_",savenameaffix,".rds"))

full.coefs.df = data.frame(
  variable = rownames(model.full.out$coeffs),
  coef.full = as.vector(model.full.out$coeffs))

#Add odds ratio
full.coefs.df = full.coefs.df %>% mutate(or.full = exp(coef.full))

results.full = data.frame(country = testdata.full$country, 
                     risk_1or2.from2022 = model.full.out$risk) %>%
  mutate(risk_rank.from2022 = rank(desc(risk_1or2.from2022), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2022))

# For last year (2021-based) predictions
model.full.2021.out = elogit.fit(ytrain=traindata.full.2021$mk_onset_1or2, 
                            Xtrain = traindata.full.2021,
                            Xtest = testdata.full.2021,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

model.full.2021.out$coeffs

full.2021.coefs.df = data.frame(
  variable = rownames(model.full.2021.out$coeffs),
  coef.full.2021 = as.vector(model.full.2021.out$coeffs))

#Add odds ratio
full.2021.coefs.df = full.2021.coefs.df %>% 
  mutate(or.full.2021 = exp(coef.full.2021))

results.full.2021 = data.frame(country = testdata.full.2021$country, 
                          risk_1or2.from2021 = model.full.2021.out$risk) %>%
  mutate(risk_rank.from2021 = rank(desc(risk_1or2.from2021), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2021))

#View(results.full.2021)

### Put together outputs in desired format ====

# first get one-year ahead forecast
model.1yr.out = elogit.fit(ytrain=traindata.full$mk_onset_1, 
                            Xtrain = traindata.full,
                            Xtest = testdata.full,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

results.1yr = data.frame(country = testdata.full$country, 
                         risk.2023only.from2022 = model.1yr.out$risk) %>%
  mutate(riskrank.2023only.from2022 = rank(desc(risk.2023only.from2022), na.last = "keep")) 

# Now merge everything up
bothforecasts <- results.full %>%
  left_join(results.1yr, by = "country")

# Merge in the prior year forecasts

bothforecastsandprior = bothforecasts %>% left_join(results.full.2021, by="country")

# Now merge that with dataset
finalresults <- bothforecastsandprior %>%
  left_join(testdata.full, by = "country")

finalresults = finalresults %>% 
  rename(risk.2023and2024.from2022 = risk_1or2.from2022,
         rank.2023and2024.from2022 = risk_rank.from2022,
         risk.2022and2023.from2021 = risk_1or2.from2021, 
         rank.2022and2023.from2021 = risk_rank.from2021)

View(finalresults)

# Round as per Vincent's request
finalresults <- finalresults %>%
  mutate(risk.2023and2024.from2022=signif(risk.2023and2024.from2022, 4),
         risk.2022and2023.from2021=signif(risk.2022and2023.from2021, 4),
         risk.2023only.from2022=signif(risk.2023only.from2022, 4))


# Now add 2021-based forecasts for comparison in prior year
write.csv(finalresults, file=paste0("final_forecasts_data_",savenameaffix,".csv"))

# Next model
# Construct training and test data, pausing to look at missingness.
traindata.alt = alldata %>% filter(year<=2020) %>% 
  select(all_of(c(predictornames.alt, outcomenames, extravariables)))
nrow(traindata.alt)
table(traindata.alt$year)

#now remove missing to compare
traindata.alt = traindata.alt %>% filter(complete.cases(.))
nrow(traindata.alt)
table(traindata.alt$year)

testdata.alt = alldata %>% filter(year==2022) %>% 
  select(all_of(c(predictornames.alt, extravariables)))

nrow(testdata.alt)

testdata.alt = testdata.alt %>% filter(complete.cases(.))
nrow(testdata.alt)

model.alt.out = elogit.fit(ytrain=traindata.alt$mk_onset_1or2, 
                             Xtrain = traindata.alt,
                             Xtest = testdata.alt,
                             alpha = .5,
                             usepredictors = predictornames.alt)

model.alt.out$coeffs

alt.coefs.df = data.frame(
  variable = rownames(model.alt.out$coeffs),
  coef.alt = as.vector(model.alt.out$coeffs))

#Add odds ratio
alt.coefs.df = alt.coefs.df %>% mutate(or.alt = exp(coef.alt))

results.alt = data.frame(country = testdata.alt$country, 
                           risk_1or2.alt = model.alt.out$risk) %>%
  mutate(risk_rank.alt = rank(desc(risk_1or2.alt), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.alt))

#View(results.alt)


### Model 3: No previous year onset indicator ====

# Construct training and test data, pausing to look at missingness.
traindata.noprev = alldata %>% filter(year<=2020) %>% 
  select(all_of(c(predictornames.noprev, outcomenames, extravariables)))
nrow(traindata.noprev)
table(traindata.noprev$year)

#now remove missing to compare
traindata.noprev = traindata.noprev %>% filter(complete.cases(.))
nrow(traindata.noprev)
table(traindata.noprev$year)

testdata.noprev = alldata %>% filter(year==2022) %>% 
  select(all_of(c(predictornames.noprev, extravariables)))

nrow(testdata.noprev)

testdata.noprev = testdata.noprev %>% filter(complete.cases(.))
nrow(testdata.noprev)

model.noprev.out = elogit.fit(ytrain=traindata.noprev$mk_onset_1or2, 
                            Xtrain = traindata.noprev,
                            Xtest = testdata.noprev,
                            alpha = .5,
                            usepredictors = predictornames.noprev)

noprev.coefs.df = data.frame(
  variable = rownames(model.noprev.out$coeffs),
  coef.noprev = as.vector(model.noprev.out$coeffs))

#Add odds ratio
noprev.coefs.df = noprev.coefs.df %>% mutate(or.noprev = exp(coef.noprev))

results.noprev = data.frame(country = testdata.noprev$country, 
                          risk_1or2.noprev = model.noprev.out$risk) %>%
  mutate(risk_rank.noprev = rank(desc(risk_1or2.noprev), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.noprev))

#View(results.noprev)


### Merge the results for comparison ====

# merge coefficients
all_coefs <- full.coefs.df %>%
  full_join(alt.coefs.df, by="variable") %>%
  full_join(noprev.coefs.df, by="variable") 
# %>% full_join(fomchange.coefs.df, by = "variable") %>%
#  full_join(nofom.coefs.df, by="variable") %>%

all_coefs <- all_coefs %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

#make the sparsity more evident
all_coefs <- all_coefs %>%
  mutate(across(where(is.numeric), ~ifelse(. == 0.000, "0", .)))

print(all_coefs)
write.csv(file=paste0("coefficients_",savenameaffix, ".csv"), all_coefs)

# Compile forecasts and save
merged_results <- results.full %>%
  full_join(results.alt, by="country") %>%
  full_join(results.noprev, by="country") 
#%>% full_join(results.fomchange, by = "country") %>%
#  full_join(results.nofom, by = "country") %>%
 
merged_results <- merged_results %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

merged_results = merged_results %>% arrange(desc(risk_1or2.from2022))
View(merged_results)
write.csv(file=paste0("forecasts_3models_",savenameaffix,".csv"), 
          merged_results)


### Setup FOOS testing regime ====

# Create a more generic training-testing function:
traintestyear = function(varnames, lasttrainyear, dvname="mk_onset_1or2"){
  
  traindata = alldata %>% filter(year<=lasttrainyear) %>% 
    select(all_of(c(varnames, outcomenames, extravariables))) %>% 
    filter(complete.cases(.))
  
  testdata = alldata %>% filter(year==lasttrainyear+2) %>% 
    select(all_of(c(varnames, extravariables, "mk_onset_1or2"))) %>%
    filter(complete.cases(.))
  
  trueoutcomes = alldata %>% filter(year==lasttrainyear+2) %>%
    select(mk_onset_1or2)
 
  ytrain = traindata$mk_onset_1or2

  model.out = elogit.fit(ytrain=ytrain, 
                             Xtrain = traindata,
                             Xtest = testdata,
                             alpha = .5,
                             usepredictors = varnames)
  
  minidata = data.frame(risk=model.out$risk, 
                        country=testdata$country, 
                        yearfrom = lasttrainyear+2,
                        ytrue=testdata$mk_onset_1or2)
  
  minidata = minidata %>% mutate(riskrank = rank(-risk, ties.method = "min"))
  
  recalls = getrecalls(data=minidata)
  #R = data.frame(year=lasttrainyear+2, recalls=recalls, minidata=minidata)
  R = list()
  R$year= lasttrainyear+2
  R$recalls = recalls
  R$minidata = minidata
  return(R)
}

getrecalls = function(data){
  numevents = sum(data$ytrue)
  
  riskdiff.annual = mean(data$risk[data$ytrue==1])-mean(data$risk[data$ytrue==0])
  riskratio.annual = mean(data$risk[data$ytrue==1])/mean(data$risk[data$ytrue==0])
  
  caught10 = data %>% arrange(desc(risk)) %>% slice(1:10) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  caught20 = data %>% arrange(desc(risk)) %>% slice(1:20) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  caught30 = data %>% arrange(desc(risk)) %>% slice(1:30) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  R = data.frame(numevents=numevents, caught10=caught10, 
                 caught20=caught20,caught30=caught30, 
                 riskdiff.annual=riskdiff.annual, riskratio.annual=riskratio.annual)
  return(R)
}

#traintestyear(varnames=predictornames.fullest, dvname = "sdads",lasttrainyear =1986)

# Define a vector of values to iterate over
# 1987 commonly used.
lasttrainyear.series <- seq(1991, 2018, by=1)

# Run with full new model (model 1)
#recallcheck_full <- data.frame(year = numeric(), 
#                        numevents = numeric(),
#                        caught10 = numeric(),
#                        caught20 = numeric(),
#                        caught30 = numeric())

recall_list = list()

FOOSdatalist = list()

varnamesnow = predictornames.fullest
#varnamesnow = predictornames.alt
#varnamesnow = predictornames.noprev
#savenameaffix = "4Oct2023_neither"

for (j in 1:length(lasttrainyear.series)){
  lasttrainyear=lasttrainyear.series[j]
  #print(lasttrainyear)
  
  thisout = traintestyear(lasttrainyear = lasttrainyear, 
                    varnames=varnamesnow, dvname = "sdads")
  #recallcheck_full = rbind(recallcheck_full,thisout)
  
  FOOSdatalist[[j]] = thisout$minidata
  
  recall_list[[j]] = thisout$recalls
}
              
FOOSdata_all = do.call(rbind, FOOSdatalist)
recalldata_all = do.call(rbind, recall_list)

FOOSdata_all = FOOSdata_all %>% rename(year = yearfrom)
#View(recallcheck_full) 

### Use two year mins/maxes as would-be forecasts.
yearmin = min(FOOSdata_all$year)+1
yearmax = max(FOOSdata_all$year)+2

allforecasts = alldata %>% filter(year>=yearmin & year<=yearmax) %>% 
 select(year, country, ccode, mk_onset)

# Add new columns for the previous and two years prior
allforecasts <- allforecasts %>% 
  mutate(prev_year = year - 1,
         two_years_prior = year - 2)

# Get "risk" and "rank" from the previous year
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(country, year, risk, riskrank), 
            by = c("country" = "country", "prev_year" = "year")) %>% 
  rename(risk_prev = risk, 
         rank_prev = riskrank)

# Get "risk" and "rank" from two years prior
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(country, year, risk, riskrank), 
            by = c("country" = "country", "two_years_prior" = "year")) %>% 
  rename(risk_two_years_prior = risk, 
         rank_two_years_prior = riskrank)

# Print the modified onsetdata with new columns

allforecasts = allforecasts %>% 
  mutate(maxrisk2 = pmax(risk_prev, risk_two_years_prior, na.rm=TRUE)) %>%
  mutate(minrank2 = pmin(rank_prev, rank_two_years_prior, na.rm=TRUE))

View(allforecasts)

# Just the onsets, for recall purposes:
onsetforecasts = allforecasts %>% filter(mk_onset==1)

write.csv(allforecasts, file=paste0("FOOSforecasts_",savenameaffix,".csv"))

# Recall 30 on this:
mean(onsetforecasts$minrank2<=30)

# Recall "averaging by year":
yearlyrecall30 = onsetforecasts %>% group_by(year) %>% 
  summarize(yearrecall30=mean(minrank2<=30))

View(yearlyrecall30)
table(yearlyrecall30$yearrecall30)
mean(yearlyrecall30$yearrecall30, na.rm=T)

# A plot of (max)risk in y=1 vs. y=0 cases
distplot = ggplot(allforecasts, aes(x = maxrisk2, fill = as.factor(mk_onset))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("No onset follows", "Onset follows")) +
  labs(fill = "Group",
       x = "Risk",
       y = "Density",
       title = "Density of Risk by Group",
       subtitle = "Grouped by onset following") +
  theme_minimal()

#print(distplot)
ggsave(paste0("distplotFOOS_maxrisk2_",savenameaffix,".pdf"), plot = distplot, width=7, height=5, dpi=300)


## Other metrics using the max/min2 approach ====
(risk.mean.y1 = mean(allforecasts$maxrisk2[allforecasts$mk_onset==1],na.rm=T))
(risk.mean.y0 = mean(allforecasts$maxrisk2[allforecasts$mk_onset==0],na.rm=T))
(risk.median.y1 = median(allforecasts$maxrisk2[allforecasts$mk_onset==1],na.rm=T))
(risk.median.y0 = median(allforecasts$maxrisk2[allforecasts$mk_onset==0],na.rm=T))


(risk.mean.diff = risk.mean.y1 - risk.mean.y0)
(risk.median.diff = risk.median.y1 - risk.median.y0)

(risk.mean.ratio = risk.mean.y1/risk.mean.y0)
(risk.median.ratio = risk.median.y1/risk.median.y0)

# Plot empirical CDF of risk for countries with onsets
fractioncaught_025 = (1-ecdf(onsetforecasts$maxrisk2)(0.025))

catchplot = ggplot(onsetforecasts, aes(x = maxrisk2)) +
  stat_ecdf(geom = "step", colour = "blue") +
  labs(title = "Empirical CDF of Risk for Countries with onsets after",
       x = "risk",
       y = "fraction caught") +
  theme_minimal() +
  scale_x_reverse() +
  geom_vline(xintercept = 0.025, linetype="dotted", color = "red") +
  geom_hline(yintercept = fractioncaught_025, linetype="dotted", color = "red") +
  annotate("text", x = 0.025, y = fractioncaught_025, label = sprintf("%.2f", fractioncaught_025), 
           hjust = -0.5, vjust = 0.5, color = "red")

print(catchplot)
ggsave(filename = paste0("catchplot_",savenameaffix,".pdf"), catchplot)


# Fraction of true among those at or above 4%"
onsetforecasts %>% 
  summarise(fraction_at_or_over_0.04 = mean(maxrisk2 >= 0.04, na.rm = TRUE),
            fraction_at_or_over_0.025 = mean(maxrisk2 >= 0.025, na.rm = TRUE))

# Get false positive per true positive. Could get from that table or by:
meanytrueatthreshold = allforecasts %>% filter(maxrisk2>.04) %>% 
  summarize(meany = mean(mk_onset, na.rm=TRUE))
# false positive to true positive
1/meanytrueatthreshold - 1

# Create a sample data frame
# Create the ROC curve
roc_curve <- roc(allforecasts$mk_onset, allforecasts$maxrisk2)

# Print the AUC
cat("AUC:", auc(roc_curve), "\n")

# Plot the ROC curve
plot(roc_curve, main="ROC Curve", col="blue", lwd=2)


### Plot overall onsets by time
yearly_sums <- alldata %>% 
  group_by(year) %>% 
  summarise(sum_mk_onset = sum(mk_onset_1or2, na.rm = TRUE))

ggplot(yearly_sums, aes(x=year, y=sum_mk_onset)) +
  geom_point() + 
  geom_smooth(method="loess") +
  labs(title="Sum of mk_onsets over years",
       x="Year",
       y="Sum of mk_onsets")
