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
          hinctnta, gndr,
          domicil) %>% 
  drop_na () %>% 
  mutate (
    z_age = scale(agea) [,1],     #scaling continuous variables
    z_edu = scale(eduyrs) [,1],
    z_wrclmch = scale(wrclmch) [,1],
    z_ccrdprs = scale(ccrdprs) [,1],
    urban = factor(ifelse(domicil <= 3, 1, 0)),  #dummy variable for 1 = urban residence, 0 = rural residence
    bctprd = factor(if_else(bctprd == 1, 1, 0)) #recoding of levels -> 0 = No, 1 = Yes
  )

## SAVING PROCESSED DATA IN A NEW FILE
#write_dta(data, "data/processed/processed_data.dta")


# OPERATIONALIZATION ----------------------------------------
## Climate concern index ------------------------------------

cor_climate <- cor(data$z_wrclmch,
                   data$z_ccrdprs)

# correlation = 0.46

data <- data %>% 
  mutate(
    clmcnrn = (z_wrclmch + z_ccrdprs) / 2
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

pca_eigen <- eigen(cor_pol) 

eigen_values <- pca_eigen$values


### Index building ------------------------------------------

data <- data %>% 
  mutate(
    pltsprt = (trstplt + trstprl + stfgov) / 3
  )

