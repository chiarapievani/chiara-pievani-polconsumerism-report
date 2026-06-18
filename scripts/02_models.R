# Empty model to see ICC to decide whether to cluster based on countries --------------------

empty_model <- glmer(
  bctprd ~ 1 + (1 | cntry),
  data = data,
  family = binomial(link = "logit")
)

sjPlot::tab_model(empty_model,
                  title = "Table 1. Empty logistic multilevel model predicting boycott participation")
