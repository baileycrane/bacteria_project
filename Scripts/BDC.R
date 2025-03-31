#' First, read in the data found in the data folder. 

read.csv('/users/baileycrane/downloads/Bailey DOC_CDOM (3).csv') 

bdc <- read.csv('/users/baileycrane/downloads/Bailey DOC_CDOM (3).csv')

##Dependencies 
install.packages('vegan')
library(vegan)
library(car) 
library(ggplot2)
install.packages('GGally')
library(GGally)


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
ggplot(bdc_sub, aes(x = Temp..deg.C., y = Bacteria.per.mL)) +
  geom_point(aes(color = Site)) +  # Add points, color by Site
  geom_smooth(method = lm) +
  labs(title = "Bacteria per mL vs Temperature",
       x = "Temperature (°C)",  
       y = "Bacteria per mL") +
  theme_minimal() +  # A clean theme
  theme(legend.position = "top")  # Position the legend at the top

ggplot(bdc_sub, aes(x = C..uM., y = Bacteria.per.mL)) +
  geom_point(aes(color = Site)) +  # Add points, color by Site
  geom_smooth(method = lm) +
  labs(title = "Bacteria per mL vs DOC (uM)",
       x = "DOC (uM)",  
       y = "Bacteria per mL") +
  theme_minimal() +  # A clean theme
  theme(legend.position = "top")  # Position the legend at the top

ggplot(bdc_sub, aes(x = CDOM..320.nm., y = Bacteria.per.mL)) +
  geom_point(aes(color = Site)) +  # Add points, color by Site
  geom_smooth(method = lm) +
  labs(title = "Bacteria per mL vs CDOM (320 nm)",
       x = "CDOM (320 nm)",  
       y = "Bacteria per mL") +
  theme_minimal() +  # A clean theme
  theme(legend.position = "top")  # Position the legend at the top


#'Generate linear models for both response variables, use bdc_sub as data
lm_bact <- lm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L. + 
                 Site, data = bdc_sub) #assume normal distribution
summary(lm_bact)
Anova(lm_bact, tupe = 3)
RsquareAdj(lm_bact)

lm_HNA <- lm(HNA.Bact ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L. + 
                Site, data = bdc_sub)
summary(lm_HNA)
Anova(lm_HNA, type = 3)
RsquareAdj(lm_HNA)

par(mfrow= c(2,2))
termplot(lm_HNA, se = TRUE, partial.resid = TRUE)

boxplot(HNA.Bact ~ Site, data = bdc_sub)

confint(lm_HNA)
?TukeyHSD

#'Run a Tukey HSD post-hoc analysis to analyze differences between sites
mod_aov <-  aov(HNA.Bact ~  Site, data = bdc_sub)
TukeyHSD(mod_aov) #FC1 significantly different from all other sites

##For the proportion of HNA out of the overall community, Site FC1 was significant
##with FC1 having such a high significance that when sites are combined, the variable
##Site is still significant.



##take out the N and P for these

#'Run Analyses for AR3

bdc_AR3$Bacteria.per.mL <- as.factor(bdc_AR3$Bacteria.per.mL)
pairs(~ bdc_AR3$Temp..deg.C. + bdc_AR3$C..uM. + bdc_AR3$CDOM..320.nm. + bdc_AR3$Chlor.a..ug.L. 
      + bdc_AR3$Bacteria.per.mL + bdc_AR3$HNA.Bact, data = bdc_AR3)

plot(bdc_AR3$Temp..deg.C., bdc_AR3$Bacteria.per.mL) 

bdc_AR3$Bacteria.per.mL <- as.numeric(bdc_AR3$Bacteria.per.mL)
cor_AR3 <- cor.test(bdc_AR3$C..uM., bdc_AR3$Bacteria.per.mL)
cor_AR3

lm_AR3 <- glm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_AR3)
summary(lm_AR3)
Anova(lm_AR3)
RsquareAdj(lm_AR3)


#'Run Analyses for FB1

bdc_FB1$Bacteria.per.mL <- as.factor(bdc_FB1$Bacteria.per.mL)
pairs(~ bdc_FB1$Temp..deg.C. + bdc_FB1$C..uM. + bdc_FB1$CDOM..320.nm. 
      + bdc_FB1$Bacteria.per.mL + bdc_FB1$HNA.Bact, data = bdc_FB1)

lm_FB1 <- glm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_FB1)
summary(lm_FB1)
Anova(lm_FB1)
RsquareAdj(lm_FB1)

#'Run Analyses for FC1

bdc_FC1$Bacteria.per.mL <- as.factor(bdc_FC1$Bacteria.per.mL)
pairs(~ Temp..deg.C. + Bacteria.per.mL + HNA, data = bdc_FC1)

lm_FC1 <- glm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_FC1)
summary(lm_FC1)
Anova(lm_FC1)
RsquareAdj(lm_FC1)



#'Run Analyses for JIC1

bdc_JIC1$Bacteria.per.mL <- as.factor(bdc_JIC1$Bacteria.per.mL)
pairs(~ bdc_JIC1$Temp..deg.C. + bdc_JIC1$C..uM. + bdc_JIC1$CDOM..320.nm. 
      + bdc_JIC1$Bacteria.per.mL + bdc_JIC1$HNA.Bact, data = bdc_JIC1)

lm_JIC1 <- glm(Bacteria.per.mL ~ C..uM. + Temp..deg.C. + Chlor.a..ug.L., data = bdc_JIC1)
summary(lm_JIC1)
Anova(lm_JIC1)
RsquareAdj(lm_JIC1)

