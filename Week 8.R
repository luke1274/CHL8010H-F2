library(here)
library(plm)

data <- source("Armed Conflict/R/merge_datasets.R")


lmmod <- lm(Maternal_Mortality_Rate ~ -1 + conflict + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = finaldata)

plmmod <- plm(Maternal_Mortality_Rate ~ conflict + gdp1000 + OECD + popdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)


summary(lmmod)
summary(plmmod)





#New models for panel data, notice how "year" is treated as the dummy variable
lmmod <- lm(Maternal_Mortality_Rate ~ -1 + conflict + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = finaldata)


plmmod <- plm(Maternal_Mortality_Rate ~ conflict + gdp1000 + OECD + popdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)


summary(lmmod)
summary(plmmod)


preds <- as.formula(" ~ -1 + conflict + gdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

matmormod <- lm(update.formula(preds, Maternal_Mortality_Rate ~ .), data = finaldata)
un5mormod <- lm(update.formula(preds, Under5_Mortality_Rate ~ .), data = finaldata)
infmormod <- lm(update.formula(preds, Infant_Mortality_Rate ~ .), data = finaldata)
neomormod <- lm(update.formula(preds, NeoNatal_Mortality_Rate ~ .), data = finaldata)


