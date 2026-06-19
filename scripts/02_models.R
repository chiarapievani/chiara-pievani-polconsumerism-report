# Empty model to see ICC to decide whether to cluster based on countries --------------------

empty_model <- glmer(
  bctprd ~ 1 + (1 | cntry),
  data = data,
  family = binomial(link = "logit")
)

sjPlot::tab_model(empty_model,
                  title = "Table 1. Empty logistic multilevel model predicting boycott participation")

# Model 1 - separate predictors ----------------------------------------

model_1a <- glmer(
  bctprd ~ polintr + pbldmna + pltsprt + lrscale + 
  (1 | cntry), data = data,
  family = binomial(link = "logit")
  )

model_1b <- glmer(
  bctprd ~ clmcnrn + (1 | cntry),
  data = data, family = binomial(link = "logit")
)

# Model 2 - all predictors + controls --------------------------

model_2 <- glmer(
  bctprd ~ polintr + pbldmna + pltsprt + lrscale + 
  clmcnrn + (1 | cntry) +
  z_age + z_edu + gndr + rsdnc, 
  data = data, family = binomial(link = "logit")
)

# Model 3 - random slopes and interaction --------------------

model_3 <- glmer(
  bctprd ~ polintr + pbldmna + pltsprt + lrscale + 
  clmcnrn + (1 + clmcnrn | cntry) + lrscale:clmcnrn +
  z_age + z_edu + gndr + rsdnc, 
  data = data, family = binomial(link = "logit")
)


table_mod1 <- tab_model(model_1a, model_1b,
          transform = NULL,
          title = "Table 2. Regression coefficients for political and environmental predictors with random intercepts for countries")

table_mod2 <- tab_model(model_2,
          transform = NULL,
          title = "Table 3. Regression coefficients for all individual predictors with random intercepts for countries")

table_mod3 <- tab_model(model_3,
                        transform = NULL,
                        title = "Table 4. Regression coefficients for full multilevel logistic regression model with random slopes and intercepts for climate concern across countries and interaction between climate concern and ideological placement")




