# takes in a data.table version of VDEM and a list of variables to keep and renames and otherwise formats variables. Drops unused variables

format_vdem <- function(dat, keep = c("COWcode", "country_name", "year", used_vars)){
  colkeep = intersect(keep, colnames(dat))
  dat <- subset(dat, select = colkeep)
  add_placeholders = setdiff(keep, colnames(dat))
  dat[, (add_placeholders) := NA]
  dat$sftgcode <- cowtopitfit(cowcodedata = dat$COWcode, yeardata=dat$year)
  
  ### Some renaming to make things easier to remember:
  names(dat)[names(dat) == 'v2elrstrct'] <- 'candidaterestriction'
  names(dat)[names(dat) == 'v2psparban_ord'] <- 'partyban'
  names(dat)[names(dat) == 'v2psoppaut_ord'] <- 'barrierstoparties'
  names(dat)[names(dat) == 'v2jureform_ord'] <- 'judicialreform'
  names(dat)[names(dat) == 'v2clrelig_ord'] <- 'religiousfreedom'
  names(dat)[names(dat) == 'v2xcl_disc'] <- 'freediscussion'
  names(dat)[names(dat) == 'v2pepwrses'] <- 'ses_power_dist'
  
  # GDP, population, trade variables
  dat$gdppcgrowth = dat$e_migdpgro #this var is also missing; have to merge in from vdem v11
  dat$popsize = dat$e_pop * 10000   #2022 update switched to e_pop as it has lower missingness than e_mipopula 
  dat$popsize.ln = log(dat$popsize)
  dat$gdppc = dat$e_gdppc*1000 #2022 update switched to e_gdppc as e_migdppc is no longer available
  dat$tradeshare = 10^6*(dat$e_cow_exports+dat$e_cow_imports)/(dat$gdppc*dat$popsize)
  dat$tradeshare.ln = log(dat$tradeshare)
  
  
  # other variables
  dat$pol_killing_approved = as.numeric(dat$v2clkill_ord==0)
  dat$freemove_men4 = as.numeric(dat$v2cldmovem_ord==4)
  dat$freemove_women4 = as.numeric(dat$v2cldmovew_ord==4)
  dat$social_inequality = as.numeric(dat$v2clsocgrp_ord==0)
  dat$even_civilrights = as.numeric(dat$v2clrgunev_ord==2)
  dat$repress_civilsoc = as.numeric(dat$v2csreprss_ord==0)
  dat$social_power_dist = as.numeric(is.element(el=dat$v2pepwrsoc_ord, set=c(0,1,2)))
  dat$minorityrule = 0
  dat$minorityrule[dat$v2pepwrsoc_ord<=1]=1
  dat$partyban.new.0 = as.numeric(is.element(el=dat$partyban, set = c(0, 1)))
  dat$partyban.new.1 = as.numeric(is.element(el=dat$partyban, set = c(2, 3)))
  dat$partyban.new.2 = as.numeric(dat$partyban==4)
  dat$v2csgender_binary = as.numeric(is.element(el=dat$v2csgender_ord, set=c(0,1,2)))
  dat$v2mecenefm_binary = as.numeric(is.element(el=dat$v2mecenefm_ord, set = c(0,1,2)))
  
  # dat <- subset(dat, select = -grep("v2", colnames(dat)))
  dat
}
