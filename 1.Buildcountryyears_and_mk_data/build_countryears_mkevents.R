#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This code takes in the master spreadsheet of events and country 
# names/codes/years to build the data framework of country-years
# and the DV (mkonset) as well as other MK event related variables
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readxl)
library(dplyr)
library(stringr)
setwd("/Users/nmbryce/Documents/R/SRA/2023-Statistical-Risk-Assessment-github/1.Buildcountryyears_and_mk_data/")
# 1. Read in country information. ====

# I had tried this with the COW countries, but I think the Gleditch
# are a better fit. So I'm switching to that.

### COW version (having made some changes to the excel sheet)
# install.packages("readxl")
#countryinfo <- read_excel("EWP mass killing events master list_cjh.xlsx", sheet = "cow_states")

# Gleditsch version
countryinfo <- read_excel("EWP mass killing events master list_cjh.xlsx", sheet = " Gleditsch ind states")
nrow(countryinfo)

# Drop any country that *ended* in 1945 or before. 
countryinfo <- countryinfo %>% filter(end_year_2017>1945)
nrow(countryinfo)
countryinfo <- countryinfo %>% rename(ccode = COW_number)

# If the country lasted until 2017, when the data stopped, 
# assume it lasted until 2022. 
# Exceptions will need to be made by hand as if updating the data.
countryinfo <- countryinfo %>% 
  mutate(end_year = ifelse(end_year_2017 < 2017, end_year_2017, 2022)) %>%
  mutate(start_year = ifelse(entry_year<1945, 1945, entry_year))


# 1.2 Compare to the set of country years we have been working with ====
legacy_countryinfo = read.csv("legacy_countryyears_29Aug2023.csv")

# Inspect first year of each country (by COWcode)
#legacy_countryinfo %>% group_by(COWcode) %>% slice_min(order_by = year, n = 1) %>% View()


# What whole countries are in our legacy set that we are missing in our new frame?
sum(is.na(legacy_countryinfo$COWcode))
missingfromours = setdiff(legacy_countryinfo$COWcode, countryinfo$ccode)
legacy_countryinfo %>% filter(COWcode %in% missingfromours) %>% select(country_name) %>% unique()

# The missing ccodes in the current data correspond to Germany and Yemen. 
# In the legacy data these had various ccodes:
legacy_countryinfo %>% 
  filter(country_name %in% c("Germany","Yemen")) %>% 
  select(country_name, COWcode, year) %>% arrange(COWcode, year) %>% View()

# In the current data, we have Germany and Yemen but with different names ccodes:
# countryinfo %>% 
#   filter(str_detect(COW_name, "Yemen")) %>% 
#   select(COW_name, ccode, start_year, end_year) %>% 
#   View()
# 
# countryinfo %>% 
#   filter(str_detect(COW_name, "German")) %>% 
#   select(COW_name, ccode, start_year, end_year) %>% 
#   View()

# So we have choices to make about how to handle these two.
# Decision for Germany: 
# Gleiditsch data had two countries called "Germany".
# The one that continues through was given cow code 255, 
# which is considered modern day Germany. The one that ends in 1990
# was given code 260.  This appears to be wrong; east Germany/GDR ought to be 265.
# In the new data, 260 is used for GFR (West Germany) and unified Germany.
# So I will start with the new data and: (i) change "German Federal Republic" 
# to be "Germany", and change it to 255 to match COW. I will rename
# GDR as "Germany (east)" and leave it with ccode 265.

countryinfo_old = countryinfo # preserve original for double checking

countryinfo = countryinfo %>% 
  mutate(COW_name = if_else(COW_name == "German Federal Republic", "Germany", COW_name)) %>% 
  mutate(COW_name = if_else(COW_name == "German Democratic Republic","Germany (East)", COW_name)) %>% 
  mutate(ccode = if_else(ccode == 260, 255, ccode))

# Verify intended renaming
#countryinfo %>%  filter(str_detect(COW_name, "German")) %>% select(COW_name, ccode, start_year, end_year) %>% View()


# What whole countries are in our new set that were not in the legacy set?
missingfromlegacy = setdiff(countryinfo$ccode, legacy_countryinfo$COWcode)
countryinfo %>% filter(ccode %in% missingfromlegacy) %>% select(COW_name) %>% unique()

# These were missing from the legacy set: 
# Bahamas, Barbados, Belize, Serbia, Iceland, Cape Verde, Zanzibar, Tibet, Brunei
# Some of these (e.g. Bahamas, Belize, Brunei) are getting close to 500k, but not yet.
# We will have to keep an eye on these.
# Zanzibar and Tibet I think we do not consider independent.
# Cape Verde may have passed 500k in about 2007 so we should discuss including.
# Serbia is not actually missing in our new data; it has a COWcode of 340 in our 
# current data  whereas it was coded to 345 in the legacy data, which 
# is Yugoslavia in the new data.

# Before proceeding, remove those from the new data (via ccode)
countryinfo = countryinfo %>% filter(!ccode %in% missingfromlegacy)

length(unique(countryinfo_old$ccode))
length(unique(countryinfo$ccode))

# 1.3 Reconcile differences in years ====

# First, we have been using country-year for the legacy data, but let's 
# collapse it down:

legacy_countryinfo_country = legacy_countryinfo %>% 
  group_by(COWcode) %>%
  summarize(
    country_name_legacy = first(country_name),
    min_year_legacy = min(year),
    max_year_legacy = max(year), 
  ) %>%
  rename(ccode_legacy = COWcode)

#View(legacy_countryinfo_country)

# Merge with the new countryinfo:
merged_countryinfo_full <- full_join(countryinfo, legacy_countryinfo_country, by = c("ccode" = "ccode_legacy"))
#View(merged_countryinfo_full)

# Make comparison easier:
merged_countryinfo_full = merged_countryinfo_full %>% 
  select(ccode, COW_name, start_year, end_year, country_name_legacy, min_year_legacy, max_year_legacy) %>%
  rename(min_year_new = start_year, max_year_new = end_year)

#View(merged_countryinfo_full)

# Highlight discrepancies:
merged_countryinfo_full = merged_countryinfo_full %>% 
  mutate(start_discrepancy = min_year_new - min_year_legacy,
         end_discrepancy = max_year_new - max_year_legacy)


# Check where the start or end years disagree:
disagreements = merged_countryinfo_full %>% 
  filter(min_year_new != min_year_legacy | max_year_new != max_year_legacy) %>%
  arrange(desc(abs(start_discrepancy)))

#View(disagreements)

#The big disagreements are in small countries and so probably correspond to 
# population size issues.  Then there are small and medium disagreements
# that must correspond to years of independence. 

# FOR PRESENT PURPOSES
# we will stick with the timings in the legacy data. But these can be revisited
# in the future. 

# We can remove the only things that didn't match: the Germany and Yemen
# rows formed by ccodes we aren't using anymore:
usecountryinfo = merged_countryinfo_full %>% filter(!is.na(COW_name))

# Create "use_start_year" and "use_end_year" to reflect the choice we will use:
usecountryinfo = usecountryinfo %>% 
  mutate(use_start_year = min_year_legacy,
        use_end_year = max_year_legacy)


# Exception:  For "Germany" (255) we need to move start date back to 1945.
usecountryinfo[usecountryinfo$ccode==255,"use_start_year"]=1945 

# Fix Yemen: in the old data there was one country running through with cow code 679,
# which is similar to how V-Dem handles it. COW codes also exist though for 
# three Yemens -- essentially a north, south, and modern. 
# In countryinfo we have two Yemens (with longer names), one running all the way through,
# One running only 1967-1990. We will do three Yemens, almost like COW: 
# (i) 678/Yemen (North) until 1989, (ii) South Yemen 1967-1989, and (iii) 
# unified Yemen (679) 1990 onward.

# The Gleiditsch data already has 678 and 680 with the right start and end years.
# We just need to add 679, starting in 1990.
usecountryinfo = usecountryinfo %>% add_row(ccode=679, COW_name="Yemen (unified)", 
                        "use_start_year"=1990, "use_end_year"=2022)

# Exception: change Sri Lanka to start in 1948, not 1972. 
usecountryinfo[stringr::str_detect(usecountryinfo$COW_name, "Sri Lanka"), "use_start_year"] <- 1948

# Exception: change Zimbabwe (Rhodesia) (552) to be independent in 1965, not 1980.
usecountryinfo[stringr::str_detect(usecountryinfo$COW_name, "Zimbabwe"), "use_start_year"] <- 1965


# Change start year to 1934 for anything present in 1945, for later purposes
usecountryinfo = usecountryinfo %>%
  mutate(use_start_year = ifelse(use_start_year == 1945, 1934, use_start_year))

# 2. Build country-year framework ====
data_mk <- data.frame()

# Loop through each row (country) in the begin_end_years data frame
for(i in 1:nrow(usecountryinfo)) {
  ccode <- usecountryinfo$ccode[i]
  #stateabv <- usecountryinfo$COW_abv[i]
  country <- as.character(usecountryinfo$COW_name[i])
  datastartyear <- as.integer(usecountryinfo$use_start_year[i])
  dataendyear <- as.integer(usecountryinfo$use_end_year[i])
  
  # Create a data frame for each country with its specific year range
  temp_df <- expand.grid(year = datastartyear:dataendyear, country = country)

  temp_df$ccode = ccode
  temp_df$country = country
  temp_df$datastartyear = datastartyear
  temp_df$dataendyear = dataendyear
  
  # Append to the master data frame
  data_mk <- rbind(data_mk, temp_df)
}


# Archive for using during debugging
#data_mk_frame = data_mk

# Check
#View(data_mk)


# Check if there are multiple countries with a given ccode in any year
# data_mk %>%
#   group_by(year, ccode) %>%
#   summarise(n = n()) %>%
#   filter(n > 1) %>% View()

# No...yay!

###------------------------------------#
# 3. Code mk onset, ongoing, ever ====
###------------------------------------#

# Read the mk events data.
mk_events <- read_excel("EWP mass killing events master list_cjh.xlsx", sheet = "mk_events")

# Correct some typos:
mk_events = mk_events %>% rename(mk_responsible_state_cow = mk_repsonsible_state_cow,
                                 mk_responsible_state_inheritor1 = mk_repsonsible_state_inheritor1,
                                 mk_responsible_state_inheritor2 = mk_repsonsible_state_inheritor2)

# Check for country names in the events data that don't match
# the country names in the country year data.

missingccodes = setdiff(mk_events$mk_location_cow, data_mk$ccode)

mk_events %>% filter(mk_location_cow %in% missingccodes) %>% select(country)

### Note re: Yemen -- we won't change Ben's data here. The events occur in 678 and 680 (
# North and South); the location inheritance goes through to the unified 
# Yemen in 1990 (679); responsibility is not inherited by 679, by either.

### Zanzibar:  
# in our data structure we have only Tanzania, 
# 1961-present, cow code 510. We don't have Zanzibar (511).
# It also does not have an ISO code. As Ben noted, it was 
# during a brief moment of independence that the killing 
# occurred.  Nevertheless, adding Zanzibar as its own country
# and getting data on it for its few months of existence
# would be tricky or impossible. So I'm recoding this event 
# to Tanzania.  We could, if we want, remove it from Tanzania's
# "ever responsible" record, but I will leave it for now.

mk_events[stringr::str_detect(mk_events$country,"Zanzibar"), "mk_location_cow"] = 510
mk_events[stringr::str_detect(mk_events$country,"Zanzibar"),"mk_location_inheritor1"] = 510 #already true
mk_events[stringr::str_detect(mk_events$country,"Zanzibar"),"mk_responsible_state_cow"] = 510

mk_events %>% filter(country=="Zanzibar") %>% View()


### Serbia
# Serbia inherits responsibility (so, 345 currently)
# whereas the location, though originally in 345 for the onset, is inherited
# by Kosovo (357 as we have it).
mk_events[stringr::str_detect(mk_events$country,"Serbia"), "mk_location_cow"] = 345
mk_events[stringr::str_detect(mk_events$country,"Serbia"),"mk_location_inheritor1"] = 357 #already true
mk_events[stringr::str_detect(mk_events$country,"Serbia"),"mk_responsible_state_cow"] = 345
mk_events[stringr::str_detect(mk_events$country,"Serbia"),"mk_responsible_state_inheritor1"] = 345

mk_events %>% filter(country=="Serbia") %>% View()

#### This big one: Loop through events and fill in: ====

data_mk$mk_onset_count <- 0 #Init to 0; will be incremented to allow multiple onsets per countryyear.
data_mk$mk_offset_count <- 0 # same story as onset_count
data_mk$mk_ongoing_count <- 0
data_mk$mk_location_ever <- 0
data_mk$mk_responsible_ever <- 0 
data_mk$mk_ongoing_widescope <- 0
data_mk$mk_ongoing_code <- "" 
data_mk$mk_ethnicflag <- "" # this would concatenate ethnic targeting "flag" values

maxyear = max(unique(data_mk$year))

for(j in 1:nrow(mk_events)){
  thisevent = mk_events[j,]  
  startyear = thisevent$startyear
  mk_location_cow = thisevent$mk_location_cow
  
  # Code endyear as maxyear+1 if it hasn't ended yet -- this just 
  # avoids NA problems and does not create an offset that should not be there.
  endyear <- ifelse(is.na(thisevent$endyear), maxyear + 2, thisevent$endyear)
  #endyear = thisevent$endyear
  
  thisevent_code = mk_events[j,"mk_event_code"]

  # in case of multiple onsets per year we will just "add one"
  existingonsetthiscountryyear = data_mk[data_mk$year == startyear & data_mk$ccode == mk_location_cow, "mk_onset_count"]
  data_mk[data_mk$year == startyear & data_mk$ccode == mk_location_cow, "mk_onset_count"] = 1 + existingonsetthiscountryyear 

  # same for offsets
  existingoffsetthiscountryyear = data_mk[data_mk$year == endyear & data_mk$ccode == mk_location_cow, "mk_offset_count"]
  
  # add the new offset, it this event has ended.
  # if the endyear didn't occur yet, this silently does nothing; sneaky.
  data_mk[data_mk$year == endyear & data_mk$ccode == mk_location_cow, "mk_offset_count"] = 1 + existingoffsetthiscountryyear 
  
  #Add a "1" to the ongoing_count 
  # It reads until the endyear of this event...but if it hasn't occured yet
  # it just goes to whatever years we are up to in the data.
  priorcountvector = data_mk[data_mk$year %in% c(startyear:endyear) & data_mk$ccode==mk_location_cow,"mk_ongoing_count"]
  newcountvector = priorcountvector + 1
  data_mk[data_mk$year %in% c(startyear:endyear) & data_mk$ccode==mk_location_cow,"mk_ongoing_count"] = newcountvector
  
  # This location ever:
  #... If it was the location of an event, turn it on forever after:
  data_mk[data_mk$year %in% c(startyear:maxyear) & data_mk$ccode==mk_location_cow,"mk_location_ever"] = 1
  
  #...If it was location inheritor 1 (This will usually, but not always, be redundant with above)
  mk_loc = thisevent$mk_location_cow
  mk_loc_inheritor1 = thisevent$mk_location_inheritor1
  mk_loc_inheritor2 = thisevent$mk_location_inheritor2
  
  # Removing "mk_loc" from the location inheritance since it would otherwise
  # still be inherited by the country where it "occurred" even if the 
  # part of the country that experienced it broke off.
  any_location_inheritor = c(mk_loc_inheritor1, mk_loc_inheritor2) %>% na.omit()
  
  data_mk[data_mk$year %in% c(startyear:maxyear) & 
            data_mk$ccode %in% any_location_inheritor,"mk_location_ever"] = 1
  
  # Now inheritors of responsibility. 
  #  If it was the COW code responsible:
  mk_responsible = thisevent$mk_responsible_state_cow
  mk_responsible_inheritor_1 = thisevent$mk_responsible_state_inheritor1
  
  #It was later decided to ignore the responsible_inheritor_2 column in the data.
  #mk_responsible_inheritor_2 = thisevent$mk_responsible_state_inheritor2
  
    any_responsibility_inheritor = c(mk_responsible, 
                                   mk_responsible_inheritor_1) #, mk_responsible_inheritor_2)
  data_mk[data_mk$year %in% c(startyear:maxyear) & 
            data_mk$ccode %in% any_responsibility_inheritor,"mk_responsible_ever"] = 1
  
  # View(data_mk[data_mk$ccode %in% any_responsibility_inheritor,])

  # Add event code -- only for the mk_locationa_cow; we won't add it to other
  # forms of inheritors...this is not for stats, just for humans to look.
  rows_to_update <- which(data_mk$year >= startyear & data_mk$year <= endyear & data_mk$ccode == thisevent$mk_location_cow)

  # Updating the cells by concatenating the new value with the existing values, 
  # in case there are other ongoings recorded there.
  
  data_mk[rows_to_update, "mk_ongoing_code"] <- paste(data_mk[rows_to_update, "mk_ongoing_code"], thisevent_code, sep="; ")

  # Manage wide/narrow targeting stuff. 
  thisevent_ethnicflag = mk_events[j,"mk_number_eth_groups"]
  
  # Only follow events in this mk_location_cow
  data_mk[rows_to_update, "mk_ethnicflag"] <- paste(data_mk[rows_to_update, "mk_ethnic_flag"], thisevent_ethnicflag, sep="; ")
  }
  
# Turn mk_ongoing_count into an ongoing binary
data_mk = data_mk %>% mutate(mk_ongoing = if_else(mk_ongoing_count>0,1,0))

# Binary onset and offsets
data_mk = data_mk %>% 
  mutate(mk_onset = if_else(mk_onset_count>0,1,0)) %>%
  mutate(mk_offset = if_else(mk_offset_count>0,1,0))

                               
# Turn the concatenated ethnic targeting flags into whatever we want:
data_mk = data_mk %>%
  mutate(widetargeting = if_else(str_detect(mk_ethnicflag, "0|2"), 1, 0)) %>%
  mutate(narrowtargeting = if_else(str_detect(mk_ethnicflag, "1"), 1, 0))
  

# Completeness checks:
# How many events were found
sum(data_mk$mk_onset_count)

# How many should there be?
nrow(mk_events)

#data_mk %>% filter(mk_onset>0) %>% arrange(year) %>% View()
#data_mk %>% group_by(year) %>% summarise(numonsetsthisyear=sum(mk_onset_count)) %>% View()

a = data_mk %>% group_by(year) %>% summarise(howmanythisyear=sum(mk_onset_count))
b = mk_events %>% group_by(startyear) %>% 
  summarise(howmanythisyear=n())
plot(a, pch=16, col=4, ylim=c(0,10))
points(b, pch=3, col=2)


### Save out
saveRDS(data_mk, "basedata2023_13sept2023.rds")
write.csv(data_mk, "basedata2023_13sept2023.csv")



# 4. Important documentation ====

## 4.1. Big coding choices: ====
# Data start in 1934, for any country present in 1945, for purposes of
# FOM lags.

# Overall approach: Use Gleditsch as a starting point for countries and their years,
# then resolve discrepancies. Mostly I resolve disagreements towards the legacy
# countries and years. The disagreements get revealed through the code,
# and should be revisited. This includes differences between start and end years w
# here the "new" (Gleditsch) and "legacy" (sftg -> pitf > ushmm) frames disagree.
# For now we are just using legacy start and end years except where this
# creates a problem (like missing an onset) or we've otherwise chosen otherwise.
# (e.g. Zimbabwe, Sri Lanka; see below).
# You can view the "disagreements" data frame to see these discrepancies.

# If a country ends after 2017, it's ending is not covered by
# Gleditsch, and so we are assuming it goes until the max year.
# Will have to make exceptions by hand if they occur.

# For present purposes, I'm coding only Tanzania (510), not
# Zanzibar (511), because we don't have Zanzibar (511).
# It also does not have an ISO code. As Ben noted, it was
# during a brief moment of independence that the killing
# occurred. Nevertheless, adding Zanzibar as its own country
# and getting data on it for its few months of existence
# would be tricky or impossible. So I'm recoding this event
# to Tanzania. We could, if we want, remove it from Tanzania's
# "ever responsible" record, but I will leave it for now.

# Serbia:
# (1) Ben's original notes say to code the location as 340, inherited by 347.
# Responsibility stays with 340.
# (2) Gleditsch has 347 and calls it Kosovo (2008-onward). Gleditsch
# also has two Serbias with 340, one pre-WWI, and one starting again in
# 2006.
# (3) In our old data framework, we don't have a 340.We have a 345 named
# Serbia (2006 to present) with sftgcode=SRB; another 345 named Serbia
# but with an sftgcode of YGS for 1945-2005, and 347 for Kosovo (347)
# for 2008 onward. So effectively we have one country, called 345, which
# is YGS then SRB, and separately a Kosovo 2008 onward.
# The structure we have might not be a bad fit, though I'm concerned
# about calling Serbia and Yugoslavia by the same CCODE --
# we need to check what data this really draws in from the sources we use.
# Serbia is the largest of the new constitute countries in former YGS but
# not by a lot.
# The decision was made to have Serbia inherit responsibility (so, 345 currently)
# whereas the location, though originally in 345 for the onset, is inherited
# by Kosovo (357 as we have it).This led to a choice to exclude "mk_location"
# (the location of the onset) from the location inheritance as a general rule.
# We could revisit this. It is mainly an issue in former Yugoslovia but
# also a few other places where mk_location and mk_location_inheritance1 don't match.

# Germany:
# Old data had two countries called "Germany".
# The one that continues through was given cow code 255,
# which is considered modern day Germany. The one that ends in 1990
# was given code 260. This appears to be wrong; east Germany/GDR ought to be 265.
# In the new data, 260 is used for GFR (West Germany) and unified Germany.
# So use the Gleditsch coding as a starting point bu then:
# (i) change "German Federal Republic" to be "Germany"
# and change its ccode from 260 to 255.
# (ii) rename GDR to "Germany (east)" and leave it with ccode 265.

# Yemen
# In the old data there is one country running through with cow code 679,
# which is similar to how V-Dem handles it. COW codes also exist though for
# three Yemens -- essentially a north, south, and modern. We return to this 
# here, and it is a good match to Ben's coding.

# Sri Lanka: independence moved up to 1948, fitting Gleditsch,
# and counting an event otherwise missed.

# Zimbabwe (Rhodesia): independence moved up to 1965, fitting Gleditsch,
# and counting an event otherwise missed.

# This emphasizes the need to revisit all discrepancies
# in independence timing between our legacy data and
# Gleditsch... problems were mainly caught when there
# was an MK but there is no telling how many of the other
# disagreements might be problematic.
#
# If you go by cow code, the following appear to be present in Gleditsch but
# missing from the legacy set: Bahamas, Barbados, Belize, Serbia, Iceland,
# Cape Verde, Zanzibar, Tibet, Brunei.
# Some of these (Bahamas, Belize, Brunei) are getting close to 500k, but not yet.
# We will have to keep an eye on these.
# Zanzibar and Tibet we do not consider independent (see note above re: Zanzibar).
# Cape Verde may have passed 500k in about 2007 so we should discuss including.
# Serbia -- see above -- it is not missing but has different COW code.

# Note also that"mk_location" (the cow code where the onset is coded to occur)
# is NOT included as the "location inheritance" code, though it is often the same
# since including it would ensure the event is inherited by the country where
# it "occurred" even if the part of the country that experienced it broke off.

# We also muted "responsible_inheritor_2" as per discussion by email.

## 4.2. Variables included and needed: ====
# Checking what we need for model:
# NB: location_ever {0,1}: has the state experienced a mass killing within its territory since 1945?
# CJH: yes, now called mk_location_ever

# NB: responsible_ever {0,1}: has the state been responsible for a mass killing since 1945?
# CJH: yes, now called mk_responsible_ever

# NB: widetargeting {0,1}: 1 if the state has one or more ongoing mass killings that target multiple groups. This can include both state and nonstate led mass killings.
# CJH: yes, called widetargeting

# NB: narrowtargeting {0,1}: 1 if the state has one or more ongoing mass killings that target one group. This can include both state and nonstate led mass killings.
# CJH: yes, narrowtargeting

# NB: anymk.ongoing {0,1}: 1 if any state or nonstate led mk is ongoing in that year
# CJH: mk_ongoing. There is also a counter of ongoings, mk_ongoing_counter.   
# NB: anymk_onset {0,1}# CJH: mk_onset; there is also mk_onset_counter, which counts how many MKs onset in a given country year (which is occasionally 2 or even 3). 

