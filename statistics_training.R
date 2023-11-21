library(tidyverse)
library(infer)
library(readxl)

#chi-square independence test
j_data1 <- read_excel("jerimalai_lithics.xlsx") %>%
  filter(Material %in% c("Volcanic", "Quartzite")) %>%
  filter(Artclas %in% c("Flake", "Hshat", "FFrag"))

null_distribution <- j_data1 %>% 
  specify(Material ~ Artclas) %>% # specify the columns we want to compare
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
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
