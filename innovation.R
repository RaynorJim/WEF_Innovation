require(plyr)
require(dplyr)
require(magrittr)
require(BMS)
require(readxl)

load("./WEF.RData")

# Filter some of the observations because of missing data
d <- d[complete.cases(d),] 
d %<>% dplyr::group_by(Country) %>% dplyr::mutate(country_obs = n())

# This removes cases that have only one observation per country - in the case of this dataset only Belize
d %<>% dplyr::filter(country_obs > 1)
d$country_obs <- NULL # remove variable

# We demean data to account for possible country specific effects
a <- colnames(d)[7:ncol(d)]
b <- d %>% dplyr::group_by(Country) %>% dplyr::select_(.dots = a) %>% dplyr::mutate_each(funs(sub_mean = . - mean(.))) %>% ungroup() %>% select(-Country)
d[, 7:ncol(d)] <- b


# this runs BMS on all countries 
d_bms <- d[,7:length(d)]
m <- bms(d_bms, 
            g="BRIC", 
            mprior="uniform",
            mcmc="enumerate", 
            user.int=FALSE)


coefs_wld <- coef(m)
coefs_wld
# this runs BMS on a subset of the EU countries
d_bms_eu <- d[d$EU == 1,7:length(d)]
m_eu <- bms(d_bms_eu, 
         g="BRIC", 
         mprior="uniform", 
         mcmc="enumerate", 
         user.int=FALSE)

# get the coefficients
coefs_eu <- coef(m_eu) 
coefs_eu

# Subset the data into chunks according to the 
# five levels of World Economic Forum's stage of development

a <- plyr::dlply(d, "Stage_of_development", function(df) {
  print(unique(df$Stage_of_development))
  d_bms <- df[,7:ncol(df)]
  m <- bms(d_bms, 
           g="BRIC", 
           mprior="uniform", 
           mcmc="enumerate", 
           user.int=FALSE)
#   return(m)
  return(list(model = m, coefs = coef(m)))
})

# Get the reuslts for the five stages of development
res1 <- a$`1`
res2 <- a$`2`
res3 <- a$`3`
res4 <- a$`4`
res5 <- a$`5`

# Combine the coefficients into a single stacked data
res <- rbind(coefs_wld, coefs_eu, res1$coefs, res2$coefs, res3$coefs, res4$coefs, res5$coefs)
res
