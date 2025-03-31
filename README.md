# bact_DOC_CDOM: The Influence of DOC and CDOM on Bacterial Abundance and Activity in Charleston Harbor (Charleston, SC).

**Objective:**

The objective of this project is to conduct exploratory analysis on data
obtained as part of a Bachelor's Thesis project. Specifically, the
relationship between bacteria, dissolved organic carbon (DOC),
chromophoric dissolved organic matter (CDOM), and temperature are of
interest.

The main questions of this project are:

\- Do variations in DOC/CDOM influence bacterial community abundance?

\- Is there a relationship between the proportion of HNA bacteria at a
site and the amount of DOC/CDOM at that site?

\- Does temperature have a significant impact on bacteria community size
and relative activity?

These questions are addressed by creating linear models to analyse
variable significance and by analyzing variation between sites.

**-- Code Structure**

**Dependencies:** You'll need to install these to make sure all
analyses/figs work.

\- library(vegan)

\- library(car)

\- library(ggplot2)

\- library(ggally)

**Data:**

The data file is in .csv format, located in the data folder. Once
imported and named, the data should be sorted by site. All sites should
be excluded except for: AR3, FB1, FC1, and JIC1.

Data was collected during May-October of 2022 and 2023 by the non-profit
Charleston WaterKeeper. Sample analysis was done at the DiTullio Lab at
Grice Marine Lab. For DOC analysis, samples were prepared at GML and
sent to the Belle W. Baruch Institute for processing.

The data contains many variables due to the exploratory nature of the
analysis. For the purposes of this project, the variables of interest
are: Temperature, DOC, CDOM, Site, Chlor-a, Bacteria per mL, and
HNA:Bact. It should be noted that for model simplicity, CDOM is excluded
with DOC being used as the main carbon source.

**Files:**

The most important files are the data and scripts files where you will
find the data.csv and main project script.

**Analysis:**

First, ggplot2 was used to create visuals for variables within the data
subset. The function pairs was also used to analyze correlations between
variables. Then two linear models, assuming normal distribution, were
created for each response variable: Bacteria per mL and HNA:Bact.
Corresponding statistical analyses (ie. summary(), Anova(, type = 3))
were conducted for each model. For site comparison, a box plot was
created for each response variable with an accompanying Tukey HSD
analysis done for between site comparisons.

**Acknowledgements:**

I would like to thank Nicole Schanke, Jack DiTullio, my DiTullio
lab-mates, Charleston WK, and Dan McGlinn without which I would not have
been able to complete this portion of my Bachelor's Thesis.
