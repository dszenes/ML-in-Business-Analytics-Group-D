
# EDA variables from 1 to 30
# Import the dataset
library(fpp3)
library(readr)
library(dplyr)
library(tibble)

# Import the dataset
DatasetMLproject <- read_csv("DatasetMLproject.csv")

# Cut from column 1 to column 30 and create a variable with the number of row, could be useful.

Dataset <- DatasetMLproject %>%
  select(url:self_reference_avg_sharess) %>%
  mutate(X = seq(1, nrow(DatasetMLproject)))
  

# EDA UNIVARIATE ANALYSIS

# Let's create a dataset only with informations about topics 


Dataset2 <- Dataset %>%
  mutate(X = seq(1, nrow(Dataset))) %>%
  select(X,data_channel_is_lifestyle:data_channel_is_world)
  

# collapsing factor ( actually it is not necessary, but at the moment I do not find an easiest way)
install.packages("forcats")
library(forcats)

# Create a dataset with the topics

Dataset3<- Dataset2 %>%
  mutate(Channel_Lifestyle = as.factor(data_channel_is_lifestyle),
         Channel_Entertainment = as.factor(data_channel_is_entertainment),
         Channel_Business = as.factor(data_channel_is_bus),
         Channel_Technology = as.factor(data_channel_is_tech),
         Channel_Socialmedia = as.factor(data_channel_is_socmed),
         Channel_World = as.factor(data_channel_is_world)) %>%
  
  mutate(Lifestyle = fct_collapse(Channel_Lifestyle,
                                  "yes"= "1",
                                  "no"= "0"),
         Entertainment = fct_collapse(Channel_Entertainment,
                                  "yes"= "1",
                                  "no" = "0"),
        Business = fct_collapse(Channel_Business,
                                  "yes" = "1",
                                   "no" = "0"),
        Technology = fct_collapse(Channel_Technology,
                                  "yes" = "1",
                                  "no" = "0"),
        Socialmedia = fct_collapse(Channel_Socialmedia,
                                   "yes" = "1",
                                   "no" = "0"),
        World = fct_collapse(Channel_World,
                             "yes" = "1",
                             "no" = "0")
        ) %>%
  select(X,Lifestyle,Entertainment,Business,Technology,Socialmedia,World)



ggplot(data = Dataset3)+
geom_bar(mapping = aes( x = Business, fill= X  ), fill= "red")

ggplot(data = Dataset3)+
  geom_bar(mapping = aes(x= Lifestyle, fill= X ), fill= "green")

ggplot(data= Dataset3)+
  geom_bar(mapping = aes(x= Entertainment, fill= X ), fill= "purple")

ggplot(data = Dataset3) +
  geom_bar(mapping = aes(x= Technology, fill= X), fill= "orange")

ggplot(data = Dataset3) +
  geom_bar(mapping = aes(x= Socialmedia, fill= X), fill= "blue")

ggplot(data = Dataset3) +
  geom_bar(mapping = aes(x= World, fill= X), fill= "pink")


# Things to improve: including a proportion percentage instead of the count, then add also the other
# topic variables in the same graph.





# Number of images 

ggplot(data = Dataset) +
  geom_histogram(mapping = aes(x= num_imgs, fill= X), fill= "orange") +
  coord_cartesian(xlim=c(0,70))

# Number of videos

ggplot(data = Dataset) +
  geom_histogram(mapping = aes(x= num_videos, fill= X), fill= "red") +
  coord_cartesian(xlim=c(0,70))



Dataset %>%
  count(num_imgs)

Dataset %>%
  count(num_videos)

# Box plots on n_...,

# Number of token in the content
ggplot(Dataset,mapping = aes(x= Dataset$n_tokens_content)) +
  geom_boxplot(fill= "yellow") +
  coord_cartesian(xlim=c(0,2000))

# Number of token in the title
ggplot(Dataset,mapping = aes(x= Dataset$n_tokens_title)) +
  geom_boxplot(fill= "yellow2") +
  coord_cartesian(xlim=c(0,25))

# Rate of unique words in the content
ggplot(Dataset,mapping = aes(x= Dataset$n_unique_tokens)) +
  geom_boxplot(fill= "yellow3") +
  coord_cartesian(xlim=c(0,1))

# https://kavita-ganesan.com/what-are-stop-words/#.YHqNEJ9xc2w
# Non stop words
# The rate is 1 so 100% for every instances

# Rate of unique non-stop words in the content
ggplot(Dataset,mapping = aes(x= Dataset$n_non_stop_unique_tokens)) +
  geom_boxplot(fill= "greenyellow") +
  coord_cartesian(xlim=c(0,1))


# variables 28, 29, 30 (self_reference_...)

min(Dataset$self_reference_min_shares)
max(Dataset$self_reference_min_shares)



  
Dataset %>% 
  ggplot(mapping = aes(x= Dataset$num_keywords)) +
  geom_boxplot(fill= "greenyellow")

# MULTIVARIATE EDA

# Scatterplot
# wait some seconds to see the graph 
Dataset %>% 
  ggplot(mapping = aes(x= num_imgs, y= num_videos)) +
  geom_point(colour = "steelblue") +
  coord_cartesian(xlim=c(0,150), ylim = c(0,150)) +
  labs(y="Number of videos", x = "number of images") +
  labs(title = "Relationship between images and videos")

# There is not a linear relationship between number of images and videos

# work in progress...




