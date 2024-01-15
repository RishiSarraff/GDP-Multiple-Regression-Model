knitr::opts_chunk$set(echo = TRUE, highlight=FALSE, comment=NA,
warnings=FALSE,message=FALSE)


## DATA CLEANSING AND PROCESSING:

      ## we will use MLR since our response variable is not binary in its outcomes 
      ## of success or failure
      library(ggplot2)
      library(dplyr)
      library(corrplot)
      library(Hmisc)
      library(car)
      library(olsrr)
      library(MASS)
      
      data <- read.csv("/Users/sarra/Documents/UVA/Second Year/Fall 2023/Stat 3220/Project/Stat3220dataset.csv")
      
      
      ## Here we replace any N/A values with the average of the other values if 
      ## quantitative and the base case if qualitative, so no harm is done through
      ## assumption.
      
      data <- na.omit(data)
      
      data$Growth[39] <- "A" 
      #https://data.worldbank.org/country/CY
      
      data$Average.Age[159] <- 38.5 
      # https://www.cia.gov/the-world-factbook/field/median-age/country-comparison/
      
      ## data Pre-processing, converting from different types to expected types
      data$Total.Population <- gsub(",", "", data$Total.Population)
      ## changing population from char to numeric
      data$Total.Population <- as.numeric(data$Total.Population) 
      
      data$Density <- gsub(",", "", data$Density)
      ## changing density from char to numeric
      data$Density <- as.numeric(data$Density) 
      ## changing averageAge from int to double 
      data$Average.Age <- as.numeric(data$Average.Age) 
      
      
      
      data$GDP <- log(data$GDP) 
      ## converted the response variable to log, so histogram works.
      data$Average.Age <- log(data$Average.Age)
      
      
      ## Histogram:
      ## this needs some serious refitting and transformations of the variables.
      ggplot(data, aes(x = GDP)) +
        geom_histogram(bins = round(sqrt(165)), fill = "blue", color = "black") + 
        labs(title = "Histogram of Response Variable - GDP", x = "GDP", y = "Frequency")
      
      ## After transforming data using log function, we have histogram that is
      ## unimodal, mostly symmetrical, continuous, and very few outliers recommended
      ## by R itself, was 13 as the number of bins, so we kept it as the rounding of
      ## the square root of the number of data points.
      
      ## Reordering data for easier calling:
      
      numeric_vars <- c("Name", "Total.Population", "Growth.Rate..Value.", "Density",
                        "Fertility", "Life_Expectancy", "Under.5.Mortality.Rate", 
                        "Sex_Ratio", "Dependency", "Average.Age", "Debt")
      categorical_vars <- c("Size", "Growth", "Income", "GDP")
      
      ## converting the categorical data from characters to factors
      for(i in categorical_vars[1:3]){
        data[[i]] <- as.factor(data[[i]])
      }
      
      data <- data %>% dplyr:: select(all_of(numeric_vars), all_of(categorical_vars))



## EXPLANATORY DATA ANALYSIS:

      ## 1st step after transformations is to do some EDA:
      
      ## Quantitative x Quantitative SCATTERPLOTS
      for (i in names(data)[2:11]) {
        plot(data[, i], data$GDP, xlab = i, ylab = "GDP", 
             main = paste("The Relationship between GDP and", i))
        print(cor(data[[i]], data$GDP, use = "complete.obs"))
      }
      
      ## Qualitative x Qualitative BOXPLOTS
      
      for (i in names(data)[12:14]) {
        boxPlot <- ggplot(data, aes_string(x=i, y="GDP")) + geom_boxplot(fill="red", color = "black")+ scale_fill_brewer(palette = "Set1") + labs(x=i, y = "GDP",title = paste("The Relationship between GDP and", i)) +theme(plot.title = element_text(hjust = 0.5))
        
        print(tapply(data$GDP, data[[i]], summary))
        
        print(boxPlot)
        
      }
      
      
      
      ## Interaction Plots for Qualitative X Qualitative
      
      ## for Size and Growth
      interaction.plot(data$Size, data$Growth, data$GDP,fun=mean, 
                       trace.label="Growth", xlab="Size",ylab="GDP")
      
      table(data$Size,data$Growth)
      
      
      ## for Growth and Income
      interaction.plot(data$Growth, data$Income, data$GDP,fun=mean,
                       trace.label="Income", xlab="Growth",ylab="GDP")
      
      table(data$Growth,data$Income)
      
      
      ## for Size and Income
      interaction.plot(data$Income, data$Size, data$GDP,fun=mean,
                       trace.label="Size", xlab="Income",ylab="GDP")
      
      table(data$Size,data$Income)
      
      
      # Interaction Plots for Qualitative X Quantitative:
      
      # Total Population x Size, Income, Growth
      plot(GDP~Total.Population , col=factor(Size),data=data,
           xlab="Total Population",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Size)),
             pch = 19,
             col = factor(levels(factor(data$Size))))
      
      plot(GDP~Total.Population , col=factor(Income),data=data,
           xlab="Total Population",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Income)),
             pch = 19,
             col = factor(levels(factor(data$Income))))
      plot(GDP~Total.Population , col=factor(Growth),data=data,
           xlab="Total Population",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Growth)),
             pch = 19,
             col = factor(levels(factor(data$Growth))))
      
      # Under 5 Mortality Rate x Size, Income, Growth
      plot(GDP~Under.5.Mortality.Rate , col=factor(Size),data=data,
           xlab="Under 5 Mortality Rate",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Size)),
             pch = 19,
             col = factor(levels(factor(data$Size))))
      
      plot(GDP~Under.5.Mortality.Rate , col=factor(Income),data=data,
           xlab="Under 5 Mortality Rate",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Income)),
             pch = 19,
             col = factor(levels(factor(data$Income))))
      
      plot(GDP~Under.5.Mortality.Rate , col=factor(Growth),data=data,
           xlab="Under 5 Mortality Rate",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Growth)),
             pch = 19,
             col = factor(levels(factor(data$Growth))))
      
      
      # Sex Ratio x Size, Income, Growth
      plot(GDP~Sex_Ratio , col=factor(Size),data=data,xlab="Sex Ratio",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Size)),
             pch = 19,
             col = factor(levels(factor(data$Size))))
      
      plot(GDP~Sex_Ratio , col=factor(Income),data=data,xlab="Sex Ratio",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Income)),
             pch = 19,
             col = factor(levels(factor(data$Income))))
      
      plot(GDP~Sex_Ratio , col=factor(Growth),data=data,xlab="Sex Ratio",ylab="GDP") 
      legend("topright",
             legend = levels(factor(data$Growth)),
             pch = 19,
             col = factor(levels(factor(data$Growth))))
      
## MULTICOLLINEARITY AND VARIABLE SCREENING:
      
      ## Since we have more than 10 explanatory variables, we will run 
      ## multicollinearity tests on the quantitative variables:
      
      
      ## we will perform an analysis on the pairwise relations and then use Stepwise 
      ## regression as a variable screening technique
      gdpCorr <- round(cor(data[2:10], use = "complete.obs"), 3)
      gdpCorr
      
      corrplot(gdpCorr)
      
      ## From this we can see that there are a few pairwise relations that are of concern:
      ## 1. Life_Expectancy and Under 5 Mortality Rate --> -0.884
      ## 2. Fertility and Growth Rate Value --> 0.860
      ## 3. Fertility and Average Age --> -0.855
      ## 4. Fertility and Dependency --> 0.859
      ## 5. Average Age and Growth Rate Value --> 0.806
      
      ## We will now run a model for VIF and test for multicollinearity
      
      multiModel <- lm(GDP ~. -Name -Size - Growth -Income, data = data)
      multiModel
      
      summary(multiModel)
      
      multiModelVIF <- round(vif(multiModel), 3)
      multiModelVIF
      
      mean(multiModelVIF)
      
      ## the average VIF appears to be 7.8822, which is much higher than the 3 cutoff
      ## and there are a few quantitative factors that have an individual VIF higher
      ## than 10 such as Fertility, Dependency, and Average Age. This is enough 
      ## evidence for us to run step_wise regression and eliminate some quantitative 
      ## variables.
      
      ## Variable Screening: 
      
      ols_step_both_p(multiModel, pent=0.15, prem=0.15, details=F)
      ols_step_forward_p(multiModel, pent = 0.15, prem= 0.15, details = F)
      ols_step_backward_p(multiModel, pent = 0.15, prem = 0.15, details = F)
      
      ## After stepwise regression, we see that the remaining values are Life 
      ## Expectancy, Sex Ratio, and Average Age. We will now confirm that these 
      ## combinations still yield a unconcerning multicollinearity value, removing 
      ## dependency and fertility, which caused most of the multicollinearity, yields 
      ## a viable equation.
      
      multiModel2 <- lm(GDP ~. -Name -Size - Growth -Income -Fertility -Density 
                        -Growth.Rate..Value. -Total.Population -Under.5.Mortality.Rate
                        -Debt -Dependency, data = data)
      multiModel2
      
      summary(multiModel2)
      
      multiModelVIF2 <- round(vif(multiModel2), 3)
      multiModelVIF2
      
      mean(multiModelVIF2) # 2.517
      
      ```
      
      ```{r}
      ## Finalize Quantitative Relationships
      
      
      ## Let's look at Quantitative x Quantitative Interactions
      interactionModel <- lm(GDP ~. -Name -Size - Growth -Income -Fertility -Density 
                             -Growth.Rate..Value. -Total.Population -Under.5.Mortality.Rate
                             -Debt -Dependency + Average.Age*Life_Expectancy, data = data)
      
      summary(interactionModel) ## comparing this to multimodel, it seems that average
      ## age and life expectancy is significant
      s2interaction <- summary(interactionModel)$sigma^2
      s2interaction
      
      
      ## therefore, we can retain the old model as our best model and move onto the 
      ## qualitative predictors.
      preQualModel <- lm(GDP ~ Average.Age + Life_Expectancy + Sex_Ratio + 
                           Average.Age*Life_Expectancy, data = data)
      summary(preQualModel)
      s2 <- summary(preQualModel)$sigma^2
      s2
      
      ## Global F test and T tests for each of the predictors, prove that this is an 
      ## adequate model with valid predictors
      
## ADDING QUALITATIVE VARIABLES AND FITTING LINEAR MODELS:      
      
      ## look at the interaction plots from the EDA and decide to add 
      ## and modify from there.
      
      ## Looking at the boxplots, all three of the quantitative variables seem
      ## to have a relationship with GDP, looking further into this through 
      ## the interaction plots:
      
      ## 1) For the Qualitative x Qualitative interaction plots, it appears that
      ## there are 2 interactions that are relevant in the EDA:
      # a) Size x Growth
      # b) Growth x Income
      
      ## 2) For the Qualitative x Quantitative interaction plots, 
      ## it appears that there ## is 1 interaction that is relevant in the EDA:
      # a) Sex_Ratio x Income
      
      testModel1 <- lm(GDP ~ Size + Income + Growth, data = data)
      summary(testModel1) ## Only income is significant in this model, so size and 
      ## growth can be removed
      s2_one <- summary(testModel1)$sigma^2
      s2_one
      
      testModel2 <- lm(GDP ~ Average.Age  + Sex_Ratio + Life_Expectancy + 
                         Average.Age*Life_Expectancy + Income, data = data)
      summary(testModel2) ## shows that sex_ratio is no longer valid 
      s2_two <- summary(testModel2)$sigma^2
      s2_two
      
      eYModel <- lm(GDP ~ Average.Age + Life_Expectancy + Average.Age*Life_Expectancy
                    + Income, data = data)
      summary(eYModel)
      s2_three <- summary(eYModel)$sigma^2
      s2_three
      
      ## As shown below, the multiple R-squared value is highest for the E(y) model, 
      ## and the 2s value is the lowest for this model, making it the final model
      ## , or best model for choice
      
## NESTED F TEST:
      
      reducedModel <- lm(GDP ~ Average.Age + Life_Expectancy + 
                           Average.Age*Life_Expectancy, data = data)
      summary(reducedModel)
      anova(reducedModel, eYModel)
      
## RESIDUAL ANALYSIS:
      
      ## Performing Residual Analysis to Check Assumptions for Violation:
      
      residualPlots(eYModel, tests = F) ## for lack of fit, we dont have any violations
      
      plot(eYModel, which = c(1,2)) ## Tests the unequal variance violation through a 
      ## Q-Q Plot, no violations, but there are some weird outliers
      
      hist(residuals(eYModel), xlab = "Residuals", ylab = "Frequency", 
           main = "The Histogram of Residuals of our Quantitative Model")  
      ## Gives us a histogram to test normality --> our histogram is normal
      
      ## Performing Residual Analysis to Check Outliers and Influential Points:
      
      influencePlot(eYModel, fill = F) 
      
      plot(eYModel, which = 4)
      
      
      ## Influential Observation Analysis
      dffits(eYModel)[c(4, 128, 133, 134, 156)] 
      ## here we get the dffits and dfbetas of the influential points to do further
      ## analysis.
      
      dfbetas(eYModel)[c(4, 128, 133, 134, 156)]
      mean(dffits(eYModel))
      
      dim(data)
      
      subdata <- data[-c(4, 128, 133, 134, 156),] 
      ## we would  remove these influential points since these specific ones cross our
      ## threshold and act as outliers in terms of fitting on the plots and all.
      
      
      finalModel <- lm(GDP ~ Average.Age + Life_Expectancy + Average.Age*Life_Expectancy
                       + Income,  data = subdata)
      summary(finalModel) ## model with removed influential has much better
      ## fitting metrics compared to other model
      s2_final <- summary(finalModel)$sigma^2
      s2_final
      ## higher adjusted r squared, higher p-values, lower residual standard error,
      ## and higher F-Statistic.
      
      ## These outlier plots all show that 4 values or countries are outlier values: 
      ## 4(Angola), 128(San Marino), 133(Seychelles), 134(Sierra Leone), 156(Ukraine).
      ## To determine whther or not to remove these influential points, we extracted 
      ## the dffits and dfbetas values and comparing these, we found that all these 
      ## points vary by large amounts from the mean.

## BOX COX TRANSFORMATION:
      
      ## Box Cox Transformation
      set.seed(123)
      train_index <- sample(1:nrow(data), 0.8*nrow(data))  
      train_data <- data[train_index,]
      test_data <- data[-train_index,]
      
      boxcox_trans <- boxcox(eYModel) 
      lambda <- boxcox_trans$x[which.max(boxcox_trans$y)]
      train_data$GDP_bc <- (train_data$GDP^lambda - 1)/lambda  
      
      eYModel_bc <- lm(GDP_bc ~ Average.Age + Life_Expectancy + 
                         Average.Age*Life_Expectancy + Income,
                       data = train_data)
      
      test_data$GDP_bc <- (test_data$GDP^lambda - 1)/lambda 
      predictions <- predict(eYModel_bc, newdata = test_data)
      
      test_MSE <- mean((test_data$GDP_bc - predictions)^2)
      print(test_MSE)
      
## CONFIDENCE AND PREDICTION INTERVALS:
      
      # We can therefore, employ this new and final model for estimation and 
      ## prediction using the metrics below.
      
      # E(y) confidence interval
      conf_interval <- predict(finalModel, interval = "confidence", level = 0.95,
                               newdata = test_data)
      conf_interval
      confint(finalModel)
      
      # y prediction interval
      pred_interval <- predict(finalModel, interval = "prediction", level = 0.95,
                               newdata = test_data)
      pred_interval
      ## create a dataframe to predict certain inputs.
      
      

