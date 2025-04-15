##Dependencies 
library(vegan)
library(car) 
library(ggplot2)
library(GGally)

#' First, read in the data found in the data folder. 

bdc <- read.csv('./Data/Bailey DOC_CDOM (3).csv')

bdc$Site <- factor(bdc$Site, levels = c("FC1", "AR3", "JIC1", "FB1")) #don't know if I
#want to do this... may take out

#on partial residuals graph, may want to make by site with st error 

#'Subset data to only include Sites: AR3, FB1, FC1, and JIC1
bdc_sub <- subset.data.frame(bdc, bdc$Site %in% c("AR3", "FB1", "FC1", "JIC1"))
head(bdc_sub)

#'Variables of Interest:
#'  - DOC
#'  - Temperature
#'  - Chlorophyll a
#'  - Response:
#'    1. Bacteria per mL (cell count per mL)
#'    2. HNA% of total community (HNA:Bact)


#'Use pairs function to visualize any correlations between variables of interest
pairs(~ Bacteria.per.mL + HNA.Bact + Temp..deg.C. + 
       C..uM. + Chlor.a..ug.L., data = bdc_sub) 



#'Further Subset data by Site for site characteristic analyses
bdc_AR3 <- subset.data.frame(bdc, bdc$Site %in% c("AR3"))
bdc_FB1 <- subset.data.frame(bdc, bdc$Site %in% c("FB1"))
bdc_FC1 <- subset.data.frame(bdc, bdc$Site %in% c("FC1"))
bdc_JIC1 <- subset.data.frame(bdc, bdc$Site %in% c("JIC1"))



#'Create exploratory graphs for bdc_sub data
#'
#Generate graph for total bacteria versus temperatre for all 4 sites
ggplot(bdc_sub, aes(x = Temp..deg.C., y = Bacteria.per.mL)) +
  geom_point(aes(color = Site)) +  # Add points, color by Site
  scale_color_manual(values = c("AR3" = "chocolate1",
                                "FB1" = "firebrick2",
                                "FC1" = "chartreuse2",
                                "JIC1" = "mediumpurple1")) +
  geom_smooth(method = lm) +
  labs(title = "Bacteria per mL vs Temperature",
       x = "Temperature (°C)",  
       y = "Bacteria per mL") +
  theme_minimal() +  # A clean theme
  theme(legend.position = "top")  # Position the legend at the top

#Generate graph for total bacteria versus DOC content for all 4 sites
ggplot(bdc_sub, aes(x = C..uM., y = Bacteria.per.mL)) +
  geom_point(aes(color = Site)) +  # Add points, color by Site
  scale_color_manual(values = c("AR3" = "chocolate1",
                                "FB1" = "firebrick2",
                                "FC1" = "chartreuse2",
                                "JIC1" = "mediumpurple1")) +
  geom_smooth(method = lm) +
  labs(title = "Bacteria per mL vs DOC (uM)",
       x = "DOC (uM)",  
       y = "Bacteria per mL") +
  theme_minimal() +  # A clean theme
  theme(legend.position = "top")  # Position the legend at the top

#Generate graph for total bacteria versus CDOM for all 4 sites
ggplot(bdc_sub, aes(x = CDOM..320.nm., y = Bacteria.per.mL)) +
  geom_point(aes(color = Site)) +  # Add points, color by Site
  scale_color_manual(values = c("AR3" = "chocolate1",
                                "FB1" = "firebrick2",
                                "FC1" = "chartreuse2",
                                "JIC1" = "mediumpurple1")) +
  geom_smooth(method = lm) +
  labs(title = "Bacteria per mL vs CDOM (320 nm)",
       x = "CDOM (320 nm)",  
       y = "Bacteria per mL") +
  theme_minimal() +  # A clean theme
  theme(legend.position = "top")  # Position the legend at the top


#'Generate linear models for both response variables, use bdc_sub as data
#'
#'Generate linear model for response variable bacterial abundance
lm_bact <- lm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L. + 
                 Site, data = bdc_sub) #assume normal distribution
summary(lm_bact) #generate a summary for model variables and significances
Anova(lm_bact, type = 3) #generate a type 3 Anova for model
RsquareAdj(lm_bact) #display adjusted Rsquare

#'Analyze model predictors and partial residuals of the bact model
par(mfrow= c(2,2)) #show 4 plots at a time for the termplot
termplot(lm_bact, se = TRUE, partial.resid = TRUE) #plot individual predictors and
#their standard error (confidence intervals)

#Generate boxplot for total bacteria site comparison
ggplot(bdc_sub, aes(x = Site, y = Bacteria.per.mL, fill = Site)) +
  geom_boxplot(alpha = 0.6) + #manipulate opacity
  labs(x = "Site", y = "Bacteria per mL") +
  theme_minimal() +
  scale_fill_manual(values = c("AR3" = "chocolate1", "FB1" = "firebrick2", "FC1" = "chartreuse2", "JIC1" = "mediumpurple1" ))

#Calculate the confidence intervals for each variable in total bacteria linear model and 
#use for the Tukey HSD
confint(lm_bact)

#'Run a Tukey HSD post-hoc analysis to analyze differences in total bacteria counts between sites
#'
mod_aov1 <-  aov(Bacteria.per.mL ~  Site, data = bdc_sub) #create an aov between sites for comparison
TukeyHSD(mod_aov1) #conduct a Tukey Honest Significant Difference (HSD) test on the
#site comparison anova to determine which groups are significantly different from
#others
#no sites are significantly different from others



#'Generate linear model for response variable bacterial activity
lm_HNA <- lm(HNA.Bact ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L. + 
                Site, data = bdc_sub) #assume normal distribution
summary(lm_HNA) #generate a summary for the model
Anova(lm_HNA, type = 3) #generate a type 3 Anova
RsquareAdj(lm_HNA) #display adjusted Rsquare

#'Analyze model predictors and partial residuals of the HNA model
par(mfrow= c(2,2)) #show 4 plots at a time for the termplot
termplot(lm_HNA, se = TRUE, partial.resid = TRUE) #plot individual predictors and
#their standard error (confidence intervals)

#'Generate a boxplot to compare HNA counts between sites
ggplot(bdc_sub, aes(x = Site, y = HNA.Bact, fill = Site)) +
  geom_boxplot(alpha = 0.6) + #manipulate opacity
  labs(x = "Site", y = "HNA.Bact") +
  theme_minimal() +
  scale_fill_manual(values = c("AR3" = "chocolate1", "FB1" = "firebrick2", "FC1" = "chartreuse2", "JIC1" = "mediumpurple1" ))

#'Calculate the confidence intervals for each variable in HNA linear model and 
#'use for the Tukey HSD
confint(lm_HNA)

#'Run a Tukey HSD post-hoc analysis to analyze differences in HNA counts between sites
#'
mod_aov2 <-  aov(HNA.Bact ~  Site, data = bdc_sub) #create an anova between sites for comparison
TukeyHSD(mod_aov2) #conduct a Tukey Honest Significant Difference (HSD) test on the
#site comparison anova to determine which groups are significantly different from
#others

##For the proportion of HNA out of the overall community, Site FC1 was significant
##with FC1 having such a high significance that when sites are combined, the variable
##Site is still significant.



#'Run Analyses for AR3
#'
bdc_AR3$Bacteria.per.mL <- as.factor(bdc_AR3$Bacteria.per.mL)
pairs(~ bdc_AR3$Temp..deg.C. + bdc_AR3$C..uM. + bdc_AR3$CDOM..320.nm. + bdc_AR3$Chlor.a..ug.L. 
      + bdc_AR3$Bacteria.per.mL + bdc_AR3$HNA.Bact, data = bdc_AR3)

plot(bdc_AR3$Temp..deg.C., bdc_AR3$Bacteria.per.mL) 

bdc_AR3$Bacteria.per.mL <- as.numeric(bdc_AR3$Bacteria.per.mL)
cor_AR3 <- cor.test(bdc_AR3$C..uM., bdc_AR3$Bacteria.per.mL)
cor_AR3

lm_AR3 <- lm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_AR3)
summary(lm_AR3)
Anova(lm_AR3)
RsquareAdj(lm_AR3)


#'Run Analyses for FB1

bdc_FB1$Bacteria.per.mL <- as.factor(bdc_FB1$Bacteria.per.mL)
pairs(~ bdc_FB1$Temp..deg.C. + bdc_FB1$C..uM. + bdc_FB1$CDOM..320.nm. 
      + bdc_FB1$Bacteria.per.mL + bdc_FB1$HNA.Bact, data = bdc_FB1)

lm_FB1 <- lm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_FB1)
summary(lm_FB1)
Anova(lm_FB1)
RsquareAdj(lm_FB1)

#'Run Analyses for FC1

bdc_FC1$Bacteria.per.mL <- as.factor(bdc_FC1$Bacteria.per.mL)
pairs(~ Temp..deg.C. + Bacteria.per.mL + HNA, data = bdc_FC1)

lm_FC1 <- lm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_FC1)
summary(lm_FC1)
Anova(lm_FC1)
RsquareAdj(lm_FC1)



#'Run Analyses for JIC1

bdc_JIC1$Bacteria.per.mL <- as.factor(bdc_JIC1$Bacteria.per.mL)
pairs(~ bdc_JIC1$Temp..deg.C. + bdc_JIC1$C..uM. + bdc_JIC1$CDOM..320.nm. 
      + bdc_JIC1$Bacteria.per.mL + bdc_JIC1$HNA.Bact, data = bdc_JIC1)

lm_JIC1 <- lm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_JIC1)
summary(lm_JIC1)
Anova(lm_JIC1)
RsquareAdj(lm_JIC1)

