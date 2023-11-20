y <- c(4, 7, 12, NA) #NA is treated as missing data.
# run: ctrl+enter
mean(y) # average value of y, if there's a NA value, the mean is NA.
mean(y, na.rm = TRUE) #remove the NA value and return the average of rest values.
# ?FunctionName = help(FunctionName)

# how to install packages
# > install.packages("package_name")
# import and read the excel data
library(readxl) # import the package: readxl
read_excel("jerimalai_lithics.xlsx")

#assign the data to an object we can use
j_data <- read_excel("jerimalai_lithics.xlsx")
mean(j_data$Weight, na.rm = TRUE)

#import some packages, remember to install them before using
library(tidyverse)
library(stringr)
library(ggplot2)
library(readr)

too_many_spaces <- c("green ", "green", "blue", "dark green")
str_squish(too_many_spaces) #remove all the spaces at the start, end of each character element

bad_capitalisation <- c("Green", "green", "bLue", "BlUe")
table(bad_capitalisation)
str_to_lower(bad_capitalisation) #all values to lowercase
bad_capitalisation #original data not changed

# coerce character data to numeric

coerce <- c(1.2, 4.3, 3.3, "7.0centimers", 2.3) # all string, not number
str(coerce) # return the data structure of coerce, all character
mean(coerce, na.rm = TRUE)
coerce_fixed <- parse_number(coerce) # change all chr to numbers
coerce_fixed
mean(coerce_fixed)
str(coerce_fixed)

# manipulate the data
str(j_data)
  # %>%, pipe line symbol. The result of left command is the input of the following command
  # select(), select columns you want
  # filter(), select specific rows you want 
j_data_subset <- 
  j_data %>% #左边的结果作为输入
  select(Material, Artclas, is.numeric) %>% # select Material, Artclas and all num columns.
  filter(Weight < 100, Material %in% # 单个数据用==，多个用%in%
           c("Chert", "Volcanic")) #select rows which Weight is smaller than 100 and Material name is Chert and Volcanic.

# add and remove columns
  # mutate(), Create, modify, and delete columns
j_data_subset <- j_data_subset %>% mutate(platform_area = Platwid * Platthic) # platform_area = Platwid columns * Platthic columns
  #split-apply-combine columns
j_data_subset <- j_data_subset %>%
  group_by(Material) %>% #split the Material columns
  summarise(mean_weight = mean(Weight, na.rm = TRUE),
            mean_length = mean(Length, na.rm = TRUE),
            sd_length = sd(Length, na.rm = TRUE)) #return the mean(Weight, Length columns) and standard variation(Length) values of Chert and Volcanic

j_data %>%
  group_by(Material) %>%
  tally() %>%#count the row number
  arrange(desc(n)) #排序, asc()升序

# ggplot2: data visualization
library(ggplot2)

ggplot(j_data) + #增加图层, add layers
  aes(x = Length) + #横坐标为Length列, x coordinate is the length column
  geom_histogram() + #画出一个直方图, draw a histogram
  xlab("Length (mm)") + 
  theme_minimal(base_size = 6) + 
  scale_x_log10()

library(ggbeeswarm)
library(scales)
j_data %>% filter(Material != "NA",
                  Material %in% c("Chert", "Volcanic", "Silcrete", "Quartzite")) %>%
  mutate(Material = fct_reorder(Material, Weight)) %>% #根据Weight中位数进行对Material排序
  ggplot() + 
    aes(x = Material,
        y = Weight) + 
    geom_boxplot() +
    scale_y_log10(labels = scales::comma) + #comma function in scales package
    #coord_flip() + #flip the coordinate
    theme_minimal()
  
ggplot(j_data) +
  aes(x = Material) + 
  geom_bar() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# scatter plot
j_data %>% filter(Material %in% c("Volcanic", "Silcrete", "Quartzite")) %>%
  ggplot() + 
    aes(x = Length, y = Width) + #, colour = Material, size = Weight, shape = Artlas
    geom_point() +# scatter plot
    geom_smooth(method = "lm", se = FALSE) + 
    facet_wrap( ~ Material, scales = "free_y") +# split the plot by Meterial, each scale is its own.
    coord_fixed()

library(ggrepel)
j_data %>% filter(Material %in% c("Quartzite"), Length > 25) %>%
  ggplot() +
  aes(Length, Width) + 
  geom_point() +
  geom_text_repel(aes(label = Material)) #label the points

ggsave("my-scatter.svg", height = 5, width = 5, dpi = 600) #save the last plot

plot_silcrete <-
  j_data %>% filter(Material == "Silcrete") %>%
    ggplot() +  
      aes(Length, Width, text=paste0("Spit: ", Spit)) + # paste0 function, value show to the point
      geom_point()
plotly::ggplotly(plot_silcrete)
                