library(tidyverse)
library(infer)
library(readxl)

"
Some essential knwoledges before we writing codes.
Data Type in Statistics: Numeric(continuous, discrete), Categorical(binary, ordinal).
Here are some examples. If you want to learn more about Statistics, you can find books, blogs and online courses.
Continuous data: temperature each day(celsius degree), e.g. 11.6, 23.2, 34.1, 39.5.
Discrete data: student number of each class, e.g. 20 for class A, 23 for class B, 11 for class C.
Binary data: people who have a car, Yes or No. True or False.
Ordinal data: univerrsity rankings, e.g. t0, t1, t2, t3.

Population: the entire group we want to reasearch, e.g. all undergraduates in the nation
Sample: a subset from Population and usually randomly selected from Population. 
We use sample because the Population is too large to be included. 
If we want to know the average height of undergraduates across the country, it's so difficult for us to collect data of each one cause
the large number of students(Maybe hundreds thousands of students or even more!).

So we can just sampling from the Population and analysis the sample data. 
From my own view, Statistics is a tool for us to infer the characters of Population using characters of the Sample.
"

#null hypothesis and alternative hypothesis
"
The characters of Population are usually unknown. 
We must make hypothesis about the population's characters before sampling.
For example, one official report said the average height of nationwide undergraduate student is 1.78 meters.
We want to know whether this conclusion is true.
We can make hypothesis, which is H0: mu0 = 1.78. We called H0 null hypothesis.
And the opposite hypothesis of H0 is H1: mu0 != 1.78, called alternative hypothesis.
We assume that H0 is true, then analyse the sample to get mu1.
Sample consists of items randomly selected from population. If we sampled many times, we got mu1, mu2, mu3, mu4...
The distribution of mu of samples can be visualized in a histogram.
If mu0 locates in the center of histogram, we might think H0 is true or H0 is not wrong.
If mu0 locates in the corner of histogram or out of the figure, we might think H0 is wrong.
Two variables called alpha(α) & p-value helps us to know the credibility of H0.
Usually we set alpha(α) to 0.05, sometimes it may be a smaller value(0.01).
We get p-value after we do statistics with sample data and H0.
If the p-value is lower than alpha(α), we can reject H0.
If the p-value is higher than alpha(α), we can not reject H0.
"

#chi-square independence test
"
We want to know if there's preference in the material of some artclass. 
For example, do they prefer volcanics to make Flake?
First, let's see the data type of Material and Artclas. 
It's obvious that they're categorical data instead of numeric type.
Chi-square independence test is very suitble to tell us whether there's preference or association.
"
# load the data we want
j_data1 <- read_excel("jerimalai_lithics.xlsx") %>% # remember to install and load the package "readxl", "tidyverse"
  filter(Material %in% c("Volcanic", "Quartzite")) %>% # we select rows which Material value is "Volcanic(火山岩)" and "Quartzite(石英岩)".
  filter(Artclas %in% c("Flake", "Hshat", "FFrag")) # we select rows which Artclas value is "Flake", "Hshat" and "FFrag".
"
In j_data1, there's 297 rows. But there're many columns not necessary for chi-square analysis.
We just want to keep the Material and Artclas columns.
"
# select the Material and Artclas columns.
j_data1 <- j_data1 %>% select(Material, Artclas) # check the structure of j_data1 with str()
# calculate the observed chi-square value of j_data1 using observe()
obs_chi <- observe(j_data1, stat = "Chisq", response = Material, explanatory = Artclas) # we want to find out the association between response variable and explanatory variable
# There're many available statistic methods in stat option: "mean", "median", "sum", "sd", "prop", "count", "diff in means", "diff in medians", "diff in props", "Chisq" (or "chisq"), "F" (or "f"), "t", "z", "ratio of props", "slope", "odds ratio", "ratio of means", and "correlation"
"
We get the chi-square value of our observed data(2.63), it is the value of the sample, not that of Population.
But where is the p-value?
"
# calculate the chi-square value and p-value using chisq_test()
chisq_data <- chisq_test(j_data1, formula = Material ~ Artclas)
"The p-value is 0.268, whici is larger than 0.05. We can say there's no association between the two columns."

# common workflow for unknown distribution
null_distribution <- j_data1 %>% 
  specify(Material ~ Artclas) %>% # specify the response and explanatory variables(columns we want to compare)
  hypothesise(null = "independence") %>% # Declare a null hypothesis about variables selected in specify().
  generate(reps = 1000, type = "permute") %>% #In the context of hypothesis testing, this is a null distribution based on the result of specify() and ⁠hypothesize().⁠
  calculate(stat = "Chisq")

null_distribution %>% 
  visualise() + 
  shade_p_value(obs_stat = 2.63, direction = "right") + 
  theme_bw(base_size = 20)

j_data1 %>% group_by(Material, Artclas) %>% tally()

#ANOVA(analysis of variation)

#PCA (Principle Component Analysis), k-means
nb_data <- read.csv("cascalheira-bicho-2020-data-prepped.csv")
nb_data_rownames <- nb_data %>% column_to_rownames(var = "Sites")
library(FactoMineR)
res.pca <- PCA(nb_data_rownames, graph = FALSE)
res.pca$eig
library(factoextra)
fviz_pca_biplot(res.pca)

#RadioCarbon dating
library(rcarbon)
library(Bchron)

x <- # assign the results to x
  calibrate(x = 4200,
            errors = 30) # mention the CalCurve is intcal20
summary(x)
plot(x) # base_plot method #c14得到的时间和实际时间没有准确对应

talimbue <- read_excel("Talimbue_radiocarbon_ages.xlsx")
# sepetate age and error
talimbue_clean <- 
  talimbue %>% 
    separate_wider_delim(`Date (BP)`, delim = "±", names = c("age", "error")) %>% #backtick ``
    mutate(age = parse_number(age), error = parse_number(error)) %>%
    arrange(age)
# calibrate multiple ages
talimbue_ages_calibrated <- calibrate(talimbue_clean$age, talimbue_clean$error)
summary(talimbue_ages_calibrated)
multiplot(talimbue_ages_calibrated)
#Phase estimation
talimbue_bdensity <-
  BchronDensity(ages = talimbue_clean$age, ageSds = talimbue_clean$error,
                calCurves = rep("intcal20", nrow(talimbue_clean))) # Must set the calCurves

# Depth age curve
talimbue_clean_depth <- #convert to num and unit is meters
talimbue_clean %>%
  mutate(Depth = parse_number(Depth),
         Depth = ifelse(Depth > 10, Depth / 100, Depth))

talimbue_clean_depth <- 
  Bchronology(ages = talimbue_clean_depth$age, 
              ageSds = talimbue_clean_depth$error, 
              positions = talimbue_clean_depth$Depth,
              ids = talimbue_clean_depth$`Laboratory code (Direct AMS)`)

plot(talimbue_clean_depth, 
     dateHeight = 0.5,
     chronCol = "grey80",
     chronTransparency = 0.3) +
  theme(axis.text.x = element_text(size = 4))

#spatial archaeological map
library(tidyverse)

pottery <- read_csv("pottery.csv")

ggplot(pottery) + 
  aes(Xsugg, Ysugg) +
  geom_point() +
  coord_equal()

library(sf)
# read in the shapefile
geology <-
  st_read("geology/geology.shp")
# convert pottery to spacial data frame
pottery_sf <-
  st_as_sf(pottery, coords = c("Xsugg", "Ysugg"))
# set crs(coordinate system) for the pottery data
st_crs(pottery_sf) <- st_crs(geology)
ggplot() + geom_sf(data = geology, aes(fill = Type)) +
  scale_fill_viridis_d() +
  geom_sf(data = pottery_sf,
          aes(colour = SherdNo),
          size = 0.1) + 
  scale_fill_viridis_d()

# spacial join
pottery_in_geology <-
  geology %>%
    st_join(pottery_sf)

pottery_in_geology %>% 
  sample_n(1000) %>% 
  group_by(Type) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# area pottery density
pottery_in_geology_density <-
  pottery_in_geology %>%
    group_by(Type, Area) %>%
    summarise(n = n() ) %>%
    mutate(sherds_per_area = n / Area) %>%
    arrange(desc(sherds_per_area))

ggplot(pottery_in_geology_density) +
  aes(reorder(Type, sherds_per_area), sherds_per_area) +
  geom_boxplot() +
  coord_flip() +
  scale_y_log10(labels = scales::comma) +
  theme(axis.text.x = element_text(size = 8))
# map the density of archaeo sites by each geological zone
ggplot(pottery_in_geology_density) + 
  geom_sf(aes(fill = sherds_per_area)) +
  scale_fill_viridis_c(trans = "log", name = "Sherd Density(log transformed)") +
  theme_void() # hide coordinates and grids
