# All the needed libraries
  library("stargazer")
  library("readxl")
  library("zoo")
  library("dplyr")
  library("lubridate")
  library("purrr")
  library("ggplot2")
  library("moments")
  library("car")
  library("lmtest")
  library("corrplot")
  library("fastDummies") #used for changing categorical data into dummy variables
  library("gt") #used for formatting tables nicely when knitting
  library("GGally")
  library("psych")
  library("moments") #for testing skewness
  library("nortest")
  #installing packages
  #install.packages("gt")
  #install.packages("fastDummies")
  #install.packages("GGally")
  #install.packages("corrplot")
  #install.packages("psych")
  #install.packages("nortest")
  library(caret)

#Setting working directory for the data
  setwd("D://Bellevue College//'24 Spring//ECON 400//Week 8 - Term Paper")
  mcdata <- read_excel("cspuf2021.xls")

  
#In this section, I'll be formatting the dataset for further usage
  
  #renaming variables for easy use https://www.geeksforgeeks.org/how-to-rename-multiple-columns-in-r/
    new_names <- c("age", "sex", "race",
                   "income", "numberchroniccond", "dentalevent", "visionevent",
                   "hearingevent", "homehealthevent", "inpatientevent",
                   "medicalproviderevent","outpatientevent", "prescribemedicine",
                   "totalpayment", "medicarepayment","medicaidpayment",
                   "medicareadvantagepayment", "privateinsurancepayment", "outofpocketpayment",
                   "uncollectedliability", "otherpayment")
    names(mcdata)<- new_names
  
  #Starting with the age variable
    # Changing categorical variables to separate dummy variables that represent each group
    # Create new columns that represent each new group
      mcdata$age_g1<-c(0)
      mcdata$age_g2<-c(0)
      mcdata$age_g3<-c(0)
  
  #to delete the mistake columns 
    #mydata2 = select(mcdata, -28, -29)
    #mcdata<-mydata2
    #rm(mydata2)
  
  #Dividing the age variable amongst the 3 new columns
    mcdata$age_g1 <- ifelse(mcdata$age == 1, 1, 0)
    mcdata$age_g2 <- ifelse(mcdata$age == 2, 1, 0)
    mcdata$age_g3 <- ifelse(mcdata$age == 3, 1, 0)
  
  #sex variable
    # Changing categorical variables to 0 & 1 dummy variables within the same column
    # value of 1 = male, value of 0 = female
      mcdata$sex <- ifelse(mcdata$sex == 1, 1, 0)
      print(head(mcdata$sex, n=5))
  
  #race variable:
    #1:Non-Hispanic white                                                       
    #2:Non-Hispanic black                                                       
    #3:Hispanic                                                                 
    #4:Other
    # Create new columns that represent each new group
      mcdata$nonhispwhite<-c(0)
      mcdata$nonhispblack<-c(0)
      mcdata$hisp<-c(0)
      mcdata$otherrace<-c(0)
    #Assigning dummy variables to each column
      mcdata$nonhispwhite <- ifelse(mcdata$race == 1, 1, 0)
      mcdata$nonhispblack <- ifelse(mcdata$race == 2, 1, 0)
      mcdata$hisp <- ifelse(mcdata$race == 3, 1, 0)
      mcdata$otherrace <- ifelse(mcdata$race == 4, 1, 0)
  
  #income variable
    # 1:<$25,000                                                                 
    # 2:>=$25,000 
    # Create new columns that represent each new group
      mcdata$incomeless25<-c(0)
      mcdata$incomemoreequal25<-c(0)
    #Using if else statement to assign values
      mcdata$incomeless25 <- ifelse(mcdata$income == 1, 1, 0)
      mcdata$incomemoreequal25 <- ifelse(mcdata$income == 2, 1, 0)

  # Removing any possible N/A values
      mcdata <- na.omit(mcdata)

  # Dropping the categorical columns after switching them to dummy columns
      #https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
        drops <- c("age", "race", "income")
      # Creating a new data set after the changes
        new_mcdata= mcdata[ , !(names(mcdata)%in% drops)]
        
        
#Visualizing/viewing the data
        
        # Checking for correlation then saving it as a csv file due to the 22*22 design
        cor_matrix <- cor(select_if(new_mcdata, is.numeric))
        cor_matrix
        write.csv(cor_matrix, "cor_matrix.csv", row.names = TRUE)
        
        
        #Testing for linearity of total payment vs each variable
        par(mfrow = c(4, 4))
        dependent_var <- "totalpayment"
        independent_vars <- setdiff(names(new_mcdata), dependent_var)
        
        # Create a function to plot linearity
        par(mfrow = c(4,4)) 
        plot_linearity <- function(x, y, plot_title) {
          plot(y ~ x, data = new_mcdata, main = plot_title, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
          abline(lm(y ~ x, data = new_mcdata), col = "red")
        }
        # Loop through each independent variable and plot linearity
        for (var in independent_vars) {
          plot_title <- paste(dependent_var, "vs", var)
          plot_linearity(new_mcdata[[var]], new_mcdata[[dependent_var]], plot_title)
        }
        
        #Viewing issue with linearity via histograms
        #Histograms
        #https://www.geeksforgeeks.org/histograms-in-r-language/
        #https://bookdown.org/dli/rguide/histogram.html
        # Dental event
        hist(new_mcdata$dentalevent, main = "Dental Event", xlab = "Dental Event ($)")
        # Vision event
        hist(new_mcdata$visionevent, main = "Vision Event", xlab = "Vision Event ($)")
        # Hearing event
        hist(new_mcdata$hearingevent, main = "Hearing Event", xlab = "Hearing Event ($)")
        # Home health event
        hist(new_mcdata$homehealthevent, main = "Home Health Event", xlab = "Home Health Event ($)")
        # Inpatient event
        hist(new_mcdata$inpatientevent, main = "Inpatient Event", xlab = "Inpatient Event ($)")
        # Medical provider event
        hist(new_mcdata$medicalproviderevent, main = "Medical Provider Event", xlab = "Medical Provider Event ($)")
        # Outpatient event
        hist(new_mcdata$outpatientevent, main = "Outpatient Event", xlab = "Outpatient Event ($)")
        # Prescribe medicine
        hist(new_mcdata$prescribemedicine, main = "Prescribe Medicine", xlab = "Prescribe Medicine ($)")
        # Total payment
        hist(new_mcdata$totalpayment, main = "Total Payment", xlab = "Total Payment ($)")
        # Medicare payment
        hist(new_mcdata$medicarepayment, main = "Medicare Payment", xlab = "Medicare Payment ($)")
        # Medicaid payment
        hist(new_mcdata$medicaidpayment, main = "Medicaid Payment", xlab = "Medicaid Payment ($)")
        # Medicare Advantage payment
        hist(new_mcdata$medicareadvantagepayment, main = "Medicare Advantage Payment", xlab = "Medicare Advantage Payment ($)")
        # Private insurance payment
        hist(new_mcdata$privateinsurancepayment, main = "Private Insurance Payment", xlab = "Private Insurance Payment ($)")
        # Out-of-pocket payment
        hist(new_mcdata$outofpocketpayment, main = "Out-of-Pocket Payment", xlab = "Out-of-Pocket Payment ($)")
        # Uncollected liability
        hist(new_mcdata$uncollectedliability, main = "Uncollected Liability", xlab = "Uncollected Liability ($)")
        # Other payment
        hist(new_mcdata$otherpayment, main = "Other Payment", xlab = "Other Payment ($)")
        #Numerical data is skewed to the right due to most of them having one heavy outlier
        
        
        #Trying to eliminate skewness by using either log or sqrt forms
        # +1 is added to handle the 0 and any possible negative values
        #Sqrt was tested, but didn't handle the skewness issue so I stuck with taking the log of all the payments that were in $
        # Dental event
        new_mcdata$log_dentalevent <- log(new_mcdata$dentalevent + 1)
        hist(new_mcdata$log_dentalevent, main = "Log-Transformed Dental Event", xlab = "Log(Dental Event ($) + 1)")
        # Vision event
        new_mcdata$log_visionevent <- log(new_mcdata$visionevent + 1)
        hist(new_mcdata$log_visionevent, main = "Log-Transformed Vision Event", xlab = "Log(Vision Event ($) + 1)")
        # Hearing event
        new_mcdata$log_hearingevent <- log(new_mcdata$hearingevent + 1)
        hist(new_mcdata$log_hearingevent, main = "Log-Transformed Hearing Event", xlab = "Log(Hearing Event ($) + 1)")
        # Home health event
        new_mcdata$log_homehealthevent <- log(new_mcdata$homehealthevent + 1)
        hist(new_mcdata$log_homehealthevent, main = "Log-Transformed Home Health Event", xlab = "Log(Home Health Event ($) + 1)")
        # Inpatient event
        new_mcdata$log_inpatientevent <- log(new_mcdata$inpatientevent + 1)
        hist(new_mcdata$log_inpatientevent, main = "Log-Transformed Inpatient Event", xlab = "Log(Inpatient Event ($) + 1)")
        # Medical provider event
        new_mcdata$log_medicalproviderevent <- log(new_mcdata$medicalproviderevent + 1)
        hist(new_mcdata$log_medicalproviderevent, main = "Log-Transformed Medical Provider Event", xlab = "Log(Medical Provider Event ($) + 1)")
        # Outpatient event
        new_mcdata$log_outpatientevent <- log(new_mcdata$outpatientevent + 1)
        hist(new_mcdata$log_outpatientevent, main = "Log-Transformed Outpatient Event", xlab = "Log(Outpatient Event ($) + 1)")
        # Prescribe medicine
        new_mcdata$log_prescribemedicine <- log(new_mcdata$prescribemedicine + 1)
        hist(new_mcdata$log_prescribemedicine, main = "Log-Transformed Prescribe Medicine", xlab = "Log(Prescribe Medicine ($) + 1)")
        # Total payment
        new_mcdata$log_totalpayment <- log(new_mcdata$totalpayment + 1)
        hist(new_mcdata$log_totalpayment, main = "Log-Transformed Total Payment", xlab = "Log(Total Payment ($) + 1)")
        # Medicare payment
        new_mcdata$log_medicarepayment <- log(new_mcdata$medicarepayment + 1)
        hist(new_mcdata$log_medicarepayment, main = "Log-Transformed Medicare Payment", xlab = "Log(Medicare Payment ($) + 1)")
        # Medicaid payment
        new_mcdata$log_medicaidpayment <- log(new_mcdata$medicaidpayment + 1)
        hist(new_mcdata$log_medicaidpayment, main = "Log-Transformed Medicaid Payment", xlab = "Log(Medicaid Payment ($) + 1)")
        # Medicare Advantage payment
        new_mcdata$log_medicareadvantagepayment <- log(new_mcdata$medicareadvantagepayment + 1)
        hist(new_mcdata$log_medicareadvantagepayment, main = "Log-Transformed Medicare Advantage Payment", xlab = "Log(Medicare Advantage Payment ($) + 1)")
        # Private insurance payment
        new_mcdata$log_privateinsurancepayment <- log(new_mcdata$privateinsurancepayment + 1)
        hist(new_mcdata$log_privateinsurancepayment, main = "Log-Transformed Private Insurance Payment", xlab = "Log(Private Insurance Payment ($) + 1)")
        # Out-of-pocket payment
        new_mcdata$log_outofpocketpayment <- log(new_mcdata$outofpocketpayment + 1)
        hist(new_mcdata$log_outofpocketpayment, main = "Log-Transformed Out-of-Pocket Payment", xlab = "Log(Out-of-Pocket Payment ($) + 1)")
        # Uncollected liability
        new_mcdata$log_uncollectedliability <- log(new_mcdata$uncollectedliability + 1)
        hist(new_mcdata$log_uncollectedliability, main = "Log-Transformed Uncollected Liability", xlab = "Log(Uncollected Liability ($) + 1)")
        # Other payment
        
        
        new_mcdata$otherpayment <- new_mcdata$otherpayment + abs(min(new_mcdata$otherpayment)) + 1
        new_mcdata$log_otherpayment <- log(new_mcdata$otherpayment)
        
        new_mcdata$ihs_otherpayment <- asinh(new_mcdata$otherpayment)
        hist(new_mcdata$log_otherpayment, main = "Log-Transformed Other Payment", xlab = "Log(Other Payment ($) + 1)")
        
    #Extracting the column and rows separately from the dim() function
      dims<-dim(new_mcdata)
      num_rows<-dims[1]
      num_columns<-dims[2]
    #Printing each one separately
      print_rows<- print(paste0("Number of rows: ", num_rows))
      print_columns<- print(paste0("Number of columns: ", num_columns))
    
  #Trying to eliminate skewness by using either log or sqrt forms
      # +1 is added to handle the 0 and any possible negative values
      #Sqrt was tested, but didn't handle the skewness issue so I stuck with taking the log of all the payments that were in $
            # Dental event
              new_mcdata$log_dentalevent <- log(new_mcdata$dentalevent + 1)
              hist(new_mcdata$log_dentalevent, main = "Log-Transformed Dental Event", xlab = "Log(Dental Event ($) + 1)")
            # Vision event
              new_mcdata$log_visionevent <- log(new_mcdata$visionevent + 1)
              hist(new_mcdata$log_visionevent, main = "Log-Transformed Vision Event", xlab = "Log(Vision Event ($) + 1)")
            # Hearing event
              new_mcdata$log_hearingevent <- log(new_mcdata$hearingevent + 1)
              hist(new_mcdata$log_hearingevent, main = "Log-Transformed Hearing Event", xlab = "Log(Hearing Event ($) + 1)")
            # Home health event
              new_mcdata$log_homehealthevent <- log(new_mcdata$homehealthevent + 1)
              hist(new_mcdata$log_homehealthevent, main = "Log-Transformed Home Health Event", xlab = "Log(Home Health Event ($) + 1)")
            # Inpatient event
              new_mcdata$log_inpatientevent <- log(new_mcdata$inpatientevent + 1)
              hist(new_mcdata$log_inpatientevent, main = "Log-Transformed Inpatient Event", xlab = "Log(Inpatient Event ($) + 1)")
            # Medical provider event
              new_mcdata$log_medicalproviderevent <- log(new_mcdata$medicalproviderevent + 1)
              hist(new_mcdata$log_medicalproviderevent, main = "Log-Transformed Medical Provider Event", xlab = "Log(Medical Provider Event ($) + 1)")
            # Outpatient event
              new_mcdata$log_outpatientevent <- log(new_mcdata$outpatientevent + 1)
              hist(new_mcdata$log_outpatientevent, main = "Log-Transformed Outpatient Event", xlab = "Log(Outpatient Event ($) + 1)")
            # Prescribe medicine
              new_mcdata$log_prescribemedicine <- log(new_mcdata$prescribemedicine + 1)
              hist(new_mcdata$log_prescribemedicine, main = "Log-Transformed Prescribe Medicine", xlab = "Log(Prescribe Medicine ($) + 1)")
            # Total payment
              new_mcdata$log_totalpayment <- log(new_mcdata$totalpayment + 1)
              hist(new_mcdata$log_totalpayment, main = "Log-Transformed Total Payment", xlab = "Log(Total Payment ($) + 1)")
            # Medicare payment
              new_mcdata$log_medicarepayment <- log(new_mcdata$medicarepayment + 1)
              hist(new_mcdata$log_medicarepayment, main = "Log-Transformed Medicare Payment", xlab = "Log(Medicare Payment ($) + 1)")
            # Medicaid payment
              new_mcdata$log_medicaidpayment <- log(new_mcdata$medicaidpayment + 1)
              hist(new_mcdata$log_medicaidpayment, main = "Log-Transformed Medicaid Payment", xlab = "Log(Medicaid Payment ($) + 1)")
            # Medicare Advantage payment
              new_mcdata$log_medicareadvantagepayment <- log(new_mcdata$medicareadvantagepayment + 1)
              hist(new_mcdata$log_medicareadvantagepayment, main = "Log-Transformed Medicare Advantage Payment", xlab = "Log(Medicare Advantage Payment ($) + 1)")
            # Private insurance payment
              new_mcdata$log_privateinsurancepayment <- log(new_mcdata$privateinsurancepayment + 1)
              hist(new_mcdata$log_privateinsurancepayment, main = "Log-Transformed Private Insurance Payment", xlab = "Log(Private Insurance Payment ($) + 1)")
            # Out-of-pocket payment
              new_mcdata$log_outofpocketpayment <- log(new_mcdata$outofpocketpayment + 1)
              hist(new_mcdata$log_outofpocketpayment, main = "Log-Transformed Out-of-Pocket Payment", xlab = "Log(Out-of-Pocket Payment ($) + 1)")
            # Uncollected liability
              new_mcdata$log_uncollectedliability <- log(new_mcdata$uncollectedliability + 1)
              hist(new_mcdata$log_uncollectedliability, main = "Log-Transformed Uncollected Liability", xlab = "Log(Uncollected Liability ($) + 1)")
            # Other payment
             
              
              new_mcdata$otherpayment <- new_mcdata$otherpayment + abs(min(new_mcdata$otherpayment)) + 1
              new_mcdata$log_otherpayment <- log(new_mcdata$otherpayment)

              new_mcdata$ihs_otherpayment <- asinh(new_mcdata$otherpayment)
              hist(new_mcdata$log_otherpayment, main = "Log-Transformed Other Payment", xlab = "Log(Other Payment ($) + 1)")
              
            
              
              #Testing for linearity of total payment vs each variable again after changes
              par(mfrow = c(4, 4))
              dependent_var <- "totalpayment"
              independent_vars <- setdiff(names(new_mcdata), dependent_var)
              
              # Create a function to plot linearity
              par(mfrow = c(4,4)) 
              plot_linearity <- function(x, y, plot_title) {
                plot(y ~ x, data = new_mcdata, main = plot_title, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
                abline(lm(y ~ x, data = new_mcdata), col = "red")
              }
              # Loop through each independent variable and plot linearity
              for (var in independent_vars) {
                plot_title <- paste(dependent_var, "vs", var)
                plot_linearity(new_mcdata[[var]], new_mcdata[[dependent_var]], plot_title)
              }
              

#Regression Model          
    # ols model

        # Variables age_g3, otherrace and incomemoreequal25 have been removed due to perfect collinearity with their opposite variables
  
          
          ols1 <- lm(formula = totalpayment ~ sex + numberchroniccond + log_dentalevent+ log_visionevent+ log_hearingevent+
                       log_medicalproviderevent +log_outpatientevent + log_prescribemedicine+ log_medicarepayment+
                       log_medicaidpayment+ log_medicareadvantagepayment+ log_privateinsurancepayment + log_outofpocketpayment+
                       log_uncollectedliability+log_otherpayment+age_g1+age_g2+hisp+nonhispblack+nonhispwhite+incomeless25
                     , data = new_mcdata)
          summary(ols1)
          r1<-residuals(ols1)
          qqnorm(r1)
          qqline(r1)

          
          
          
          ols2 <- lm(formula = totalpayment ~  numberchroniccond + log_visionevent + log_medicalproviderevent + 
                       log_outpatientevent + log_prescribemedicine + log_medicarepayment + log_medicaidpayment +
                       log_medicareadvantagepayment + log_privateinsurancepayment + log_outofpocketpayment +
                       log_uncollectedliability + log_otherpayment + nonhispblack, data = new_mcdata)
          summary(ols2)
          r2<-residuals(ols2)
          qqnorm(r2)
          qqline(r2)
          
 
          library("MASS")
          new_mcdata$totalpayment <- new_mcdata$totalpayment + 1
          bc <- boxcox(ols2, lambda = seq(-3, 3))
          best.lam = bc$x[which(bc$y==max(bc$y))]
          
          # Update the model formula with the best lambda value
          fullmodel.inv <- lm((totalpayment^best.lam) ~ numberchroniccond + log_visionevent + log_medicalproviderevent + 
                                log_outpatientevent + log_prescribemedicine + log_medicarepayment + log_medicaidpayment +
                                log_medicareadvantagepayment + log_privateinsurancepayment + log_outofpocketpayment +
                                log_uncollectedliability + log_otherpayment + nonhispblack, data = new_mcdata)
          summary(fullmodel.inv)
          r3<-residuals(ols3)
          qqnorm(r3)
          qqline(r3)
          
          stargazer(ols1,ols2, ols3,  type = "text") 
          