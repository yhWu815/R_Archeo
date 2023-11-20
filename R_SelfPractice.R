# import excel files
library(readxl)
xls_data <- read_excel(readxl_example("datasets.xls")) # datasets.xls is a example in readxl package
# show the structure of datasets.xls
str(xls_data) # 150 rows, 5 columns (4 chr, 1 num)
# show the values of specific columns
sep_length <- xls_data$Sepal.Length # Sepal.Length column
length(sep_length) # see how many items in Sepal.Length column

# some statistical values we want
max(sep_length) # maxina
min(sep_length) # minina
range(sep_length) # return min, max
sd(sep_length) # standard deviation
var(sep_length) # variation
mean(sep_length) # average value

# manipulate data
  # manipulate columns: select(), mutate()
  # select(), select columns we want
library(tidyverse) # it's recommended to import tidyverse at the beginning of any projects
sepal <- xls_data %>% select(Sepal.Length, Sepal.Width, Species) 
  #before use pipeline symbol '%>%', please import tidyverse package!
petal <- xls_data %>% select(Petal.Length, Petal.Width, Species)
  # mutate(), Create, modify, and delete columns
total_length <- xls_data %>% mutate(total_length = Sepal.Length + Petal.Length)
length_ratio <- xls_data %>% mutate(length_ratio = Sepal.Length / Petal.Length)
  # manipulate rows: filter()
  # before we use filter() function, we should learn logical operators in R.
3 != 1 # ! is NOT
(3 > 1 & 1 > 2) # &, && are AND
(3 > 1 | 1 > 2) # |, || are OR 
xor(TRUE, TRUE) # elementwise exclusive, 异或。两值相同为FALSE，两值不同为TRUE
  #filter() test1: find out all sepal length of setosa(Species)
  sepal %>% select(Sepal.Length, Species) %>% filter(Species == 'setosa')
  #filter() test2: find out all sepal length of setosa and virginica(Species)
  sepal %>% select(Sepal.Length, Species) %>% filter(Species %in% c('setosa', 'virginica'))
  #filter() test3: find out all sepal length larger than the mean
  mean(xls_data$Sepal.Length)
  sepal %>% select(Sepal.Length, Species) %>% filter(Sepal.Length > mean(Sepal.Length))
  #filter() test4: find out all sepal length of versicolor larger than the mean of versicolor
  versicolor_sep_length <- xls_data %>% filter(Species == "versicolor")
  versicolor_sep_length_small <- versicolor_sep_length %>% filter (Sepal.Length > mean(Sepal.Length))
  #arange the data using arrange()
  arrange(versicolor_sep_length_small, desc(Sepal.Length)) # arrange the versicolor_sep_length_small, descening is the default sort
