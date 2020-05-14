
# ---
#   
# title: "Lab 7 - Ten Growth Models - Simple to Advanced"
# author: "Adam Garber"
# subtitle: 'Structural Equation Modeling - Instructor: Karen Nylund-Gibson'
# date: "`May 14, 2020`"
# output: 
#   
# ---


`University of California, Santa Barbara`

# ______________________________________________

# Lab preparation

# ______________________________________________

## Creating a version-controlled R-Project with Github

# Download repository here: https://github.com/garberadamc/SEM-Lab7
# 
# On the Github repository webpage:
#   
# a. `fork` your own `branch` of the lab repository 
# b. copy the repository web URL address from the `clone or download` menu
# 
# Within R-Studio:
#   
# c. click "NEW PROJECT" 
# d. choose option `Version Control`
# e. choose option `Git`
# f. paste the repository web URL path copied from the `clone or download` menu on Github page
# g. choose location of the R-Project 

# ______________________________________________

## Data source:

# The first 3 models utilize a public use data subset the *Longitudinal Survey of American Youth (LSAY)*  [$\color{blue}{\text{See documentation here}}$](https://www.lsay.org/)

# ______________________________________________

# Load packages
library(transformr)
library(gganimate)
library(hrbrthemes)
library(tidyverse)
library(haven)
library(janitor)
library(MplusAutomation)
library(rhdf5)
library(here)
library(kableExtra)
library(gtsummary)
library(semPlot)
library(naniar)
  

# ______________________________________________

## `LSAY` data example - `Math Scores` across 6 timepoints

# ______________________________________________

# Read in data 
lsay_data <- read_spss(here("data", "LSAY_labs.sav")) %>%                     
  select(RURAL, GENDER, FATHED, MOTHED,                                
         -starts_with("AB"),                                           
         ends_with("IMP"),                                             
         -contains("BIO"),                                             
         -contains("PHY")) %>%                                         
  clean_names() %>%                                                    
  rename( math_07 = amthimp ,                                          
          math_08 = cmthimp ,                                          
          math_09 = emthimp ,                                          
          math_10 = gmthimp ,                                          
          math_11 = imthimp ,                                          
          math_12 = kmthimp ,                                          
          sci_07 = asciimp ,                                           
          sci_08 = csciimp ,                                           
          sci_09 = esciimp ,                                           
          sci_10 = gsciimp ,                                           
          sci_11 = isciimp ,                                           
          sci_12 = ksciimp ) %>%                                       
  replace_with_na_all(condition = ~.x == 9999.00)                              

  

# ______________________________________________

# View metadeta 
sjPlot::view_df(lsay_data)
  
# Write a `CSV` file
write_csv(lsay_data, here("data", "lsay_lab7_data.csv"))

# Read in the `CSV` file (SPSS labels removed)
lsay_lab7 <- read_csv(here("data", "lsay_lab7_data.csv"))
  
# ______________________________________________

# Let's start modeling 

# ______________________________________________

# Table. LSAY repeated measures 

var_table <- tribble(
  ~"Name",      ~"Labels",  ~"Variable type",                                   
  #--------------|--------------------------------|-----|,
  "math_07"   , "7th grade math score  "   , "time varying covariate",
  "math_08"   , "8th grade math score  "   , "     ",
  "math_09"   , "9th grade math score  "   , "     ",
  "math_10"   , "10th grade math score "   , "     ",
  "math_11"   , "11th grade math score "   , "     ",
  "math_12"   , "12th grade math score "   , "     ",
  "         " , "                      "   , "     ",
  "sci_07"   , "7th grade science score  " , "model indicators (outcomes)",
  "sci_08"   , "8th grade science score  " , "     ",
  "sci_09"   , "9th grade science score  " , "     ",
  "sci_10"   , "10th grade science score " , "     ",
  "sci_11"   , "11th grade science score " , "     ",
  "sci_12"   , "12th grade science score " , "     ")

var_table %>% 
  kable(booktabs = T, linesep = "") %>% 
  kable_styling(latex_options = c("striped"), 
                full_width = F,
                position = "left")
  

# ______________________________________________

## Model 01 -  `Fixed time effects` 

# ______________________________________________

m1_growth  <- mplusObject(
  TITLE = "m1 growth model fixed time scores - Lab 7", 
  VARIABLE = 
    "usevar =
    sci_07-sci_12; ", 
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s | sci_07@0 sci_08@1 sci_09@2 sci_10@3 sci_11@4 sci_12@5; " ,
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m1_growth_fit <- mplusModeler(m1_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m1_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

# Load in the `mplus.R` functions
source(here("mplus.R.txt"))
  

## Plotting using `gh5` plot data generated by `Mplus`

# 1. View plots available for a given model 
# 2. Generate plots using the `get.plot.___` function
# 3. Extract data and transform to tidy format
# 4. Plot with `ggplot`

mplus.view.plots(here("mplus_files", "m1_growth_Lab6.gh5"))
  

# Prepare plot data
observed <- lsay_lab7 %>% select(starts_with("sci")) %>%
  rownames_to_column() %>% drop_na()

obs100 <- observed[1:100,]

plot_obs <- obs100 %>% 
  pivot_longer(`sci_07`:`sci_12`, # The columns I'm gathering together
               names_to = "grade", # new column name for existing names
               values_to = "value") # new column name to store values

gradelevels <- colnames(observed[,2:7])

mean_est <- as.data.frame(mplus.get.estimated_means(here(
  "mplus_files", "m1_growth_Lab7.gh5"))) %>%
  mutate(grade = gradelevels)

  

# Plot the model estimated means superimposted on the obserbed individual values 

growth_plot <- ggplot() +                                                                   #         
  geom_point(data = plot_obs,                                                               #
             aes(x = grade, y = value, group = rowname), alpha = .3) +                      #   
  geom_line(data = plot_obs,                                                                #
            aes(x = grade, y = value, group = rowname), alpha = .3) +                       #   
  geom_point(data=mean_est,                                                                 #
             aes(x=grade, y = V1), color = "Blue", size = 1.5) +                            #    
  geom_line(data=mean_est,                                                                  #
            aes(x=grade, y = V1, group = 1), color = "Blue", size = 1.2) +                  #        
  scale_x_discrete(labels = c("7", "8", "9", "10", "11", "12")) +                           #  
  labs(x="Grade", y="Science Score") +                                                      #     
  theme_minimal()                                                                              

growth_plot
  
ggsave(here("figures", "spaghetti_p1.png"), height = 6, width = 8, dpi = "retina")
  

# Animate the plot with {`gganimate`}
growth_plot +                                                                                #
  transition_states(rowname,                                                                 #
                    transition_length = 1,                                                   #
                    state_length = 1) +                                                      #
  shadow_mark(color = "Magenta", alpha = .3)                                                 # 
  
anim_save(here("figures", "spaghetti_plot.gif"), height = 6, width = 8, dpi = "retina")
  

# ______________________________________________

## Model 02 -  `Centering the Intercept`

# ______________________________________________

# a. Centering determines the interpretation of the intercept growth factor
# b. The centering point is the timepoint at which the time score is `zero`
# c. A model can be estimated for different centering points depending
# on which interpretation is of interest

m2_growth  <- mplusObject(
  TITLE = "m2 growth model centering time scores - Lab 7", 
  VARIABLE = 
    "usevar =
    sci_07-sci_12; ", 
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s | sci_07@-2 sci_08@-1 sci_09@0 sci_10@1 sci_11@2 sci_12@3; " ,
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m2_growth_fit <- mplusModeler(m2_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m2_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

## Model 03 - `freely estimated time scores`

# ______________________________________________

    

knitr::include_graphics(here("figures", "m3_free_time_L7.png"))
  

    

m3_growth  <- mplusObject(
  TITLE = "m3 growth model freely estimated time scores - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12; ", 
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s | sci_07@0 sci_08* sci_09* sci_10* sci_11* sci_12@1; " ,
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m3_growth_fit <- mplusModeler(m3_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m3_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

# Prepare plot data

mean_est2 <- as.data.frame(mplus.get.estimated_means(here(
  "mplus_files", "m3_growth_Lab7.gh5"))) %>%
  mutate(grade = gradelevels)

  

# Plot the model estimated means superimposted on the obserbed individual values 
growth_plot <- ggplot() +                                                                                       
  geom_point(data = plot_obs,
             aes(x = grade, y = value, group = rowname), color = "lightblue", alpha = .3) +                     #  
  geom_line(data = plot_obs,                                                                                    #
            aes(x = grade, y = value, group = rowname), color = "lightblue", alpha = .3) +                      #   
  geom_point(data=mean_est2,                                                                                    #   
             aes(x=grade, y = V1), color = "magenta", size = 1.5) +                                             # 
  geom_line(data=mean_est2,                                                                                     #
            aes(x=grade, y = V1, group = 1), color = "magenta", size = 1.2) +                                   #
  scale_x_discrete(labels = c("7", "8", "9", "10", "11", "12")) +                                               #  
  labs(x="Grade", y="Science Score") +                                                                          #
  theme_minimal()                                                                                               # 

growth_plot
  

# ______________________________________________

## Model 04 - `time-invariant covariates` and `freely estimated time scores` 

# ______________________________________________

covariates:
  
# - `gender`: 1 = female
# - `rural`:  1 = rural
# - `fathed`: Father's reported education
# - `mothed`: Mother's reported education

m4_growth  <- mplusObject(
  TITLE = "m4 time-invariant covariates and freely estimated time scores - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12
     gender rural fathed mothed; ", 
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s | sci_07@0 sci_08* sci_09* sci_10* sci_11* sci_12@1; 
    i s on gender rural fathed mothed;" ,
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m4_growth_fit <- mplusModeler(m4_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m4_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

# Check the path diagram with `semPlot`

m4_output <- readModels(here("mplus_files", "m4_growth_Lab7.out"))

semPaths(m4_output,  "est",                                                             # 
         intercepts=FALSE, residuals = FALSE, fade = FALSE,                             #   
         edge.color = "black", edgeLabels = "")                                         #            

# ______________________________________________

## Model 05 - `time-varying covariates` 

# ______________________________________________

# repeated measure covariate: `math scores: grades 7 to 12`
# time-invariant covariate: `mothed`

m5_growth  <- mplusObject(
  TITLE = "m05 time-varying covariates - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12
     math_07-math_12 mothed; ",
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s | sci_07@0 sci_08* sci_09* sci_10* sci_11* sci_12@1; 
    i s on mothed;
    sci_07 on math_07;
    sci_08 on math_08;
    sci_09 on math_09;
    sci_10 on math_10;
    sci_11 on math_11;
    sci_12 on math_12; ",
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m5_growth_fit <- mplusModeler(m5_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m5_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# Check the path diagram 

m5_output <- readModels(here("mplus_files", "m5_growth_Lab7.out"))

semPaths(m5_output,  "est",
         intercepts=FALSE, residuals = FALSE, fade = FALSE,
         edge.color = "black", edgeLabels = "")

  
# ______________________________________________

## Model 06 - `Time-varying covariate with time-invariant effect`

# ______________________________________________

m6_growth  <- mplusObject(
  TITLE = "m06 time-varying covariates - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12
     math_07-math_12 mothed; ",
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s | sci_07@0 sci_08* sci_09* sci_10* sci_11* sci_12@1; 
    i s on mothed;
    sci_07 on math_07(1); ! TIME-INVARIANT: Fixed to equality
    sci_08 on math_08(1);
    sci_09 on math_09(1);
    sci_10 on math_10(1);
    sci_11 on math_11(1);
    sci_12 on math_12(1); ",
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m6_growth_fit <- mplusModeler(m6_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m6_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

## Model 07 - `Quadratic Growth`

# ______________________________________________

m7_growth  <- mplusObject(
  TITLE = "m07 Quadratic Growth (i s q) - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12
     math_07-math_12; ",
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i s q | sci_07@0 sci_08@1 sci_09@2 sci_10@3 sci_11@4 sci_12@5; ",
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m7_growth_fit <- mplusModeler(m7_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m7_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

## Model 08 - `Parallel Process Growth Model`

# ______________________________________________

m8_growth  <- mplusObject(
  TITLE = "m08 Parallel Process - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12
     math_07-math_12 mothed;",
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "is ss | sci_07@0 sci_08@1 sci_09@2 sci_10@3 sci_11@4 sci_12@5; 
    im sm | math_07@0 math_08@1 math_09@2 math_10@3 math_11@4 math_12@5; 
    
    is ss im sm on mothed;  ! time-invariant covariate ",
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m8_growth_fit <- mplusModeler(m8_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m8_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

## Model 09 - `Piecewise Process Growth Model`

# ______________________________________________

m9_growth  <- mplusObject(
  TITLE = "m09 piecewise growth - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12 mothed;",
  
  ANALYSIS = 
    "estimator = MLR" ,
  
  MODEL = 
    "i1 s1 | sci_07@0 sci_08@1 sci_09@2 sci_10@2 sci_11@2 sci_12@2; 
      s2 by sci_07@0 sci_08@0 sci_09@0 sci_10@1 sci_11@2 sci_12@3; 
    
    i1 s1 s2 on mothed; ",
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m9_growth_fit <- mplusModeler(m9_growth,
                              dataout=here("mplus_files", "Lab7.dat"),       
                              modelout=here("mplus_files", "m9_growth_Lab7.inp"),
                              check=TRUE, run = TRUE, hashfilename = FALSE)
  

# ______________________________________________

## Model 10 - `Piecewise Process Growth Model`

# ______________________________________________

m10_growth  <- mplusObject(
  TITLE = "m10 piecewise growth - Lab 7", 
  VARIABLE = 
    "usevar =
     sci_07-sci_12 mothed;",
  
  ANALYSIS = 
    "estimator = MLR;" ,
  
  MODEL = 
    "i1 s1 | sci_07@0 sci_08@1 sci_09@2; 
    i2 s2 | sci_10@0 sci_11@1 sci_12@2; 
    
    [s1] (p1);
    [s2] (p2);

    i1 s1 s2 on mothed; ",
  
  MODELTEST = "p1=p2;  !testing if the two slopes are the same",
  
  OUTPUT = "sampstat standardized;",
  
  PLOT = "type=plot3;
          series = sci_07-sci_12(*)",
  
  usevariables = colnames(lsay_lab7),   
  rdata = lsay_lab7)                    

m10_growth_fit <- mplusModeler(m10_growth,
                               dataout=here("mplus_files", "Lab7.dat"),       
                               modelout=here("mplus_files", "m10_growth_Lab7.inp"),
                               check=TRUE, run = TRUE, hashfilename = FALSE)
  

# Check the path diagram 

m10_output <- suppressWarnings(readModels(here("mplus_files", "m10_growth_Lab7.out")))

semPaths(m10_output,  "est",
         intercepts=FALSE, residuals = FALSE, fade = FALSE,
         edge.color = "black", edgeLabels = "")

  

# ______________________________________________

# References

# Hallquist, M. N., & Wiley, J. F. (2018). MplusAutomation: An R Package for Facilitating Large-Scale Latent Variable Analyses in Mplus. Structural equation modeling: a multidisciplinary journal, 25(4), 621-638.
# 
# Miller, J. D., Hoffer, T., Suchner, R., Brown, K., & Nelson, C. (1992). LSAY codebook. Northern Illinois University.
# 
# Muthén, B. O., Muthén, L. K., & Asparouhov, T. (2017). Regression and mediation analysis using Mplus. Los Angeles, CA: Muthén & Muthén.
# 
# Muthén, L.K. and Muthén, B.O. (1998-2017).  Mplus User’s Guide.  Eighth Edition. Los Angeles, CA: Muthén & Muthén
# 
# R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/
#   
#   Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
# 
# ---------------------------------------------------
#   
# ![](figures/UCSB_Navy_mark.png){ width=75% }




















