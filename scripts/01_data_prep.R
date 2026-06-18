# DATA IMPORT AND PREPARATION -------------------

ess <- read_stata(here("data", "raw", "ESS_round11.dta"))

data <- ess %>%
  
  select (idno, cntry,
          
          bctprd, #dependent variable
          
          polintr, pbldmna,  #independent variables - political
          trstplt, trstprl, stfgov,  #latent factor - political support
          lrscale,
          
          wrclmch, ccrdprs,  #independent variables - climate concern
          
          agea, eduyrs,      #control variables
          gndr, domicil) %>% 
  
  drop_na () %>% 
  
  mutate (
    bctprd = factor(if_else(bctprd == 1, 1, 0)), #recoding of levels -> 0 = No, 1 = Yes
    
    polintr = (5 - polintr), #reverse scaling for political interest so that high value = high interest
    pbldmna = factor (if_else(pbldmna == 1, 1, 0)), #recoding of levels -> 0 = No, 1 = Yes
    lrscale = (10 - lrscale), #reverse scaling for left - right scale so that higher values indicate left people
    
    
    z_age = scale(agea) [,1],     #standardization of continuous variables
    z_edu = scale(eduyrs) [,1],
    urban = factor(ifelse(domicil <= 3, 1, 0)),  #dummy variable for 1 = urban residence, 0 = rural residence
    gndr = factor(ifelse(gndr == 1, 0, 1)) #recoding of levels -> 0 = Male, 1 = Female
  )


# OPERATIONALIZATION ----------------------------------------
## Climate concern index ------------------------------------

cor_climate <- cor(scale(data$wrclmch),
                   scale(data$ccrdprs))

# correlation = 0.46

data <- data %>% 
  mutate(
    clmcnrn = (scale(data$wrclmch) + scale(data$ccrdprs)) / 2
  )

## Factor analysis - political support index ---------------

pol_data <- data %>% 
  select(
    trstplt,
    trstprl,
    stfgov
  )

cor_pol <- cor(pol_data)

### Eigen values evaluation - n of dimensions to retain -----

eigen_pol <- eigen(cor_pol) [[1]]

### Index building ------------------------------------------

data <- data %>% 
  mutate(
    pltsprt = (trstplt + trstprl + stfgov) / 3
  )


#SAVING PROCESSED DATA IN A NEW FILE
write.csv(data, "data/processed/processed_data.csv", row.names = FALSE)
