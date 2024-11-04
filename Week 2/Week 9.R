library(here)
library(plm)
library(mice)

data <- source("Armed Conflict/R/merge_datasets.R")


lmmod <- lm(Maternal_Mortality_Rate ~ -1 + conflict + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = finaldata)
preds <- as.formula(" ~ -1 + conflict + log(gdp1000) + OECD + popdens + urban +
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought +
                  ISO + as.factor(year)")

matmormod <- lm(update.formula(preds, Maternal_Mortality_Rate ~ .), index = c("ISO", "year"),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)
un5mormod <- lm(update.formula(preds, Under5_Mortality_Rate ~ .), index = c("ISO", "year"),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)
infmormod <- lm(update.formula(preds, Infant_Mortality_Rate ~ .), index = c("ISO", "year"),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)
neomormod <- lm(update.formula(preds, NeoNatal_Mortality_Rate ~ .), index = c("ISO", "year"),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)

summary(neomormod)


#MI Stuff
MI_finaldata <- finaldata
midata <- MI_finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "Maternal_Mortality_Rate", "Infant_Mortality_Rate", "NeoNatal_Mortality_Rate", "Under5_Mortality_Rate", "gdp1000", "popdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "Maternal_Mortality_Rate", "Infant_Mortality_Rate", "NeoNatal_Mortality_Rate", "Under5_Mortality_Rate", "gdp1000", "popdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

plot(mice.multi.out)



#Creating new models
fit_models <- function(data) {
  list(
    matmormod = lm(Maternal_Mortality_Rate ~ ., data = data),
    un5mormod = lm(Under5_Mortality_Rate ~ ., data = data),
    infmormod = lm(Infant_Mortality_Rate ~ ., data = data),
    neomormod = lm(NeoNatal_Mortality_Rate ~ ., data = data)
  )
}

imputed_models <- vector("list", length(mice.multi.out))

for (i in seq_along(mice.multi.out)) {
  imputed_models[[i]] <- fit_models(mice.multi.out[[i]])
}

pooled_results <- lapply(imputed_models, function(models) {
  list(
    matmormod = pool(models$matmormod),
    un5mormod = pool(models$un5mormod),
    infmormod = pool(models$infmormod),
    neomormod = pool(models$neomormod)
  )
})

combined_results <- do.call(rbind, lapply(pooled_results, function(res) {
  rbind(
    tidy(res$matmormod),
    tidy(res$un5mormod),
    tidy(res$infmormod),
    tidy(res$neomormod)
  )
}))
