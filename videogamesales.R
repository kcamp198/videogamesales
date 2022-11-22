setwd("C:/Users/kcamp/AppData/Local/R/win-library/4.2")

remove.packages("caret")

install.packages("vctrs")

##### 1.1 Overview #####

## Set knitr options
knitr::opts_chunk$set(echo = TRUE)

## Create vg set, validation set (final hold-out test set), set more options, install packages

options(repos = list(CRAN="http://cran.rstudio.com/"))

knitr::opts_chunk$set(message = FALSE)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(jtools)) install.packages("jtools", repos = "http://cran.us.r-project.org")
if(!require(huxtable)) install.packages("huxtable", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(psych)
library(gridExtra)
library(jtools)
library(huxtable)

## Original data from kaggle.com user rush4ratio
## Data includes video game sales from Vgchartz and ratings from Metacritic
## Data now hosted on my dropbox

download.file("https://www.dropbox.com/s/pkkuwg8et5qdj70/Video_Games_Sales_as_at_22_Dec_2016.csv?dl=1","vg.csv", mode = "wb")

data = read.csv("vg.csv")

## Validation set will be 10% of vg data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = data$User_Score, times = 1, p = 0.1, list = FALSE)
vg <- data[-test_index,]
temp <- data[test_index,]

## Make sure Name and Platform in validation set are also in vgs set
validation <- temp %>% 
  semi_join(vg, by = "Name") %>%
  semi_join(vg, by = "Platform")

## Add rows removed from validation set back into vg set
removed <- anti_join(temp, validation)
vg <- rbind(vg, removed)

rm(data, temp, test_index, removed)

vg %>% filter(User_Score > 0) %>% nrow()

## View first ten rows of vg data
vg %>% slice(1:10) %>% knitr::kable(caption = "Table 1.1. First ten rows of vg dataset",
                                     row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Convert Critic_Score NAs to mean of Critic_Score in vg
vg$Critic_Score[is.na(vg$Critic_Score)] <- mean(vg$Critic_Score, na.rm = T)

## Make User_Score numeric
vg <- transform(vg, User_Score = as.numeric(User_Score))

## Convert User_Score NAs to mean of User_Score in vg
vg$User_Score[is.na(vg$User_Score)] <- mean(vg$User_Score, na.rm = T)

## Create vg summary table
vg_summary <- data.frame(Rows = nrow(vg),
                          Columns = ncol(vg),
                          "Unique games" = n_distinct(vg$Name),
                          "Unique platforms" = n_distinct(vg$Platform),
                          "Average global sales" = round(mean(vg$Global_Sales),2),
                          "Average critic score" = round(mean(vg$Critic_Score),2),
                          "Average user score" = round(mean(vg$User_Score),2),
                          "Number of genres" = n_distinct(vg$Genre),
                          check.names = FALSE)

## Display the first six columns of the vg summary table
vg_summary[,1:6] %>% 
  knitr::kable(caption = "Table 1.2. Summary of vg set") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Visualize sparsity using a random sample of our matrix of users, movies
install.packages("rafalib")
games <- sample(unique(vg$Name), 100)
rafalib::mypar()
vg %>% filter(Name %in% games) %>% 
  select(Name, Platform, User_Score) %>%
  mutate(User_Score = 1) %>%
  spread(Platform, User_Score) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Platform", ylab="Games")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

## Display the last two columns of the vg summary table
vg_summary[,7:8] %>% 
  knitr::kable(caption = "Table 1.3. Date range in vg set") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Display rows in vg with no genre information
vg %>%
  filter(genres == "(no genres listed)") %>% 
  knitr::kable(caption = "Table 1.4. Rows in vg lacking genre information",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

##### 2.0 Methods and analysis #####

## Create and display summary table of ratings
describe(vg$rating, fast = TRUE) %>%
  select(-vars) %>%
  knitr::kable(caption = "Table 2.1. Summary statistics for ratings",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Create a table with the sum of each user rating
rating_sum <- vg %>% group_by(rating) %>%
  summarize(count = n())

## Plot the count by rating using the rating sum table
rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Rating", y = "Count")

##### 2.1 Techniques and processes #####

## Update vg with timestamp converted to date format
vg <- vg %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01 00:00:00",tz = "GMT"))) %>% 
  mutate(rating_year = year(rating_time)) %>%
  select(-timestamp)

## View first five rows of updated vg dataset
vg %>% slice(1:5) %>% knitr::kable(caption = "Table 2.2. First five rows of updated vg dataset",
                                    row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Update vg to add release year
vg <- vg %>%
  mutate(release_year = as.integer(substr(title, str_length(title) - 4,
                                          str_length(title) - 1)))

## View first five rows of updated vg dataset
vg %>% slice(1:5) %>% knitr::kable(caption = "Table 2.3. First five rows of vg dataset with release year",
                                    row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Create and display summary table of ratings by movie
vg %>%
  dplyr::count(movieId) %>%
  describe(fast = TRUE) %>%
  select(-vars) %>%
  slice(2) %>%
  knitr::kable(caption = "Table 2.4. Summary statistics for ratings",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Visualize the distribution of ratings per movie
vg %>%
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Number of ratings", y = "Count of unique movies") + 
  scale_x_log10()

## Create quick summary table for scatterplot of average and number of ratings
movie_sum <- vg %>% group_by(movieId) %>%
  summarize(ratings = n(), 
            mu = mean(rating),
            sd = sd(rating))

## Display scatterplot of average and number of ratings
movie_sum %>% 
  ggplot(aes(ratings, mu)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  geom_vline(aes(xintercept = mean(movie_sum$ratings)), color = "red") +
  annotate("text", x = 2000, y = 5,
           label = round(mean(movie_sum$ratings),0),
           color = "red", size = 3) +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) +
  labs(x = "Number of ratings per movie",
       y = "Average rating per movie")

## Plot average rating distribution alongside overall rating distribution
plot1 <- movie_sum %>% ggplot(aes(mu)) + 
  geom_histogram(fill = "steel blue", color = "black",
                 binwidth = 0.5) +
  labs(title = "Distribution of movie average ratings",
       x = "Rating",
       y = "Count") + 
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10))

plot2 <- rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  labs(title = "Distribution of movie ratings",
       x = "Rating",
       y = "Count")

grid.arrange(plot1, plot2, ncol=2)

## Create and display summary table of ratings by user
vg %>%
  dplyr::count(userId) %>%
  describe(fast = TRUE) %>%
  select(-vars) %>%
  slice(2) %>%
  knitr::kable(caption = "Table 2.5. Summary statistics for users",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Visualize the distribution of ratings per user
vg %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Number of ratings", y = "Count of unique users") + 
  scale_x_log10()

## Create quick user summary table for scatterplot of average and number of user-ratings
user_sum <- vg %>% group_by(userId) %>%
  summarize(user_ratings = n(), 
            user_mu = mean(rating),
            user_sd = sd(rating))

## Display scatterplot of average and number of user-ratings
user_sum %>% 
  ggplot(aes(user_ratings, user_mu)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  geom_vline(aes(xintercept = mean(user_sum$user_ratings)), color = "red") +
  annotate("text", x = 400, y = 5,
           label = round(mean(user_sum$user_ratings),0),
           color = "red", size = 3) +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) +
  labs(x = "Number of ratings per user",
       y = "Average rating per user")

## Plot average rating distribution alongside overall rating distribution
plot3 <- user_sum %>% ggplot(aes(user_mu)) + 
  geom_histogram(fill = "steel blue", color = "black",
                 binwidth = 0.5) +
  labs(title = "Distribution of user-average ratings",
       x = "Rating",
       y = "Count") + 
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10))

plot4 <- rating_sum %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  labs(title = "Distribution of movie ratings",
       x = "Rating",
       y = "Count")

grid.arrange(plot3, plot4, ncol=2)

## create quick summary table for plot of the number of ratings by release year
release_year_sum <- vg %>% group_by(release_year) %>%
  summarize(n = n(), average_rating = mean(rating))

## Display plot of number of ratings by release year
ggplot(release_year_sum, aes(release_year, n)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(x = "Year",
       y = "Count") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Plot the average rating by release year
ggplot(release_year_sum, aes(release_year, average_rating)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_smooth() +
  theme_classic() +
  labs(x = "Year",
       y = "Rating") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Fit linear model of average rating on release year and release year squared
fit_lm1 <- lm(average_rating ~ I(release_year^2) +
                I(release_year), 
              data = release_year_sum)

## Fit linear model of average rating on release year, release year squared, and release year cubed
fit_lm2 <- lm(average_rating ~ I(release_year^3) + I(release_year^2) +
                I(release_year), 
              data = release_year_sum)

## Create table of results for both linear models
export_summs(fit_lm1, fit_lm2)

## Calculate the first rating time of each movie
first_sum <- vg %>% group_by(movieId) %>%
  summarize(first_ratings = n(),
            first_mu = mean(rating),
            first_rating_time = min(rating_time))

## Calculate the weeks elapsed since first rating
vg <- vg %>% left_join(first_sum, by = "movieId")
vg <- vg %>%
  mutate(weeks_elapsed = as.numeric(round((rating_time - first_rating_time)/7,0)))

## Create a summary table grouping by weeks elapsed
weeks_elapsed_sum <- vg %>% group_by(weeks_elapsed) %>%
  summarize(n_weeks_elapsed = n(),
            average_rating = mean(rating))

## Plot ratings count by weeks elapsed since first rating
ggplot(weeks_elapsed_sum, aes(weeks_elapsed, n_weeks_elapsed)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(x = "Weeks since first rating",
       y = "Count") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Plot average rating by weeks elapsed since first rating
ggplot(weeks_elapsed_sum, aes(weeks_elapsed, average_rating)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_line(color = "steel blue") +
  theme_classic() +
  labs(x = "Weeks since first rating",
       y = "Average rating") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Create vector of genres in vg
genres <- str_replace(vg$genres, "\\|.*","")

## Drop duplicate genres from the vector
genres <- genres[!duplicated(genres)]

## Create a table listing the unique genres
genres %>%
  knitr::kable(caption = "Table 2.7. List of unique genres in vg",
               col.names = "Genre",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Calculate the number of movies per genre
n_genres <- sapply(genres, function(g){
  index <- str_which(vg$genres, g)
  length(vg$rating[index])
  
})

## Calculate the average rating by genre
genres_rating <- sapply(genres, function(g){
  index <- str_which(vg$genres, g) 
  mean(vg$rating[index], na.rm = T)
})

## Create summary table by genres
genres_sum <- data.frame(Genre = genres, 
                         Movies = n_genres,
                         "Average rating" = genres_rating,
                         check.names = FALSE)

## Display summary table by genres
genres_sum %>% arrange(desc(Movies)) %>% slice(1:5) %>%
  knitr::kable(caption = "Table 2.8. Most common genres in vg",
               digits = 2,
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Plot average rating by genre
colnames(genres_sum)[3] <- "average_rating"
genres_sum %>%
  ggplot(
    aes(x = reorder(Genre, average_rating), average_rating)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  coord_flip() +
  labs(
    y = "Average rating",
    x = "Genres") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Create list of users close to the average
user_list <- user_sum %>% 
  filter(user_ratings >= round(mean(user_ratings),2)-1,
         user_ratings <= round(mean(user_ratings),2)+1) %>% 
  select(userId, user_mu)

## Sample 10 users at random
set.seed(1, sample.kind = "Rounding")
user_list <- sample(user_list$userId, 10)

## Create average rating figure by genre for the 10 random users
vg_random <- vg %>%
  filter(userId %in% user_list)

## Calculate the number of movies per genre
n_genres2 <- sapply(genres, function(g){
  index <- str_which(vg_random$genres, g)
  length(vg_random$rating[index])
})


## Calculate the average rating by genre
genres_rating2 <- sapply(genres, function(g){
  index <- str_which(vg_random$genres, g) 
  mean(vg_random$rating[index], na.rm = T)
})

## Create summary table by genres
genres_sum2 <- data.frame(Genre = genres, 
                          Movies = n_genres2,
                          average_rating = genres_rating2)
## Plot average rating by genre for random sample of 10 users
genres_sum2 %>%
  ggplot(
    aes(x = reorder(Genre, average_rating), average_rating)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  coord_flip() +
  labs(
    y = "Average rating",
    x = "Genres") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

##### 3.1 Model results #####

## Compute RMSE using the average of all ratings
rmses <- data_frame(method = "Only estimate is the average",
                    RMSE = RMSE(mean(vg$rating), vg$rating))
## Create the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.1. RMSE by method I",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add movie--average to vg
movie_sum <- vg %>% group_by(movieId) %>%
  summarize(mu_movie = mean(rating))

vg <- left_join(vg, movie_sum, by = "movieId")
## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add movie–average effect",
                              RMSE = RMSE(vg$mu_movie, vg$rating)))
## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.2. RMSE by method II",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add user--average to vg
b_u_sum <- vg %>% mutate(yhat = rating - mu_movie) %>%
  group_by(userId) %>%
  summarize(b_u = mean(yhat))

vg <- left_join(vg, b_u_sum, by = "userId") %>%
  mutate(mu_movie_user = mu_movie + b_u)
## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add user–specific effect",
                              RMSE = RMSE(vg$mu_movie_user, vg$rating)))
## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.3. RMSE by method III",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add time effect to vg
b_t_sum <- vg %>% mutate(error = rating - mu_movie_user) %>%
  group_by(weeks_elapsed) %>%
  summarize(b_t = mean(error))

vg <- left_join(vg, b_t_sum, by = "weeks_elapsed") 

## Convert NAs to 0
vg$b_t[is.na(vg$b_t)] <- 0

vg <- vg %>%
  mutate(mu_movie_user_time = mu_movie_user + b_t)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add time effect",
                              RMSE = RMSE(vg$mu_movie_user_time, vg$rating)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.4. RMSE by method IV",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add genre effect to vg
b_g_sum <- vg %>% mutate(error = rating - mu_movie_user_time) %>%
  group_by(genres) %>%
  summarize(b_g = mean(error))

vg <- left_join(vg, b_g_sum, by = "genres") 

vg <- vg %>%
  mutate(mu_movie_user_time_genres = mu_movie_user_time + b_g)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add genre effect",
                              RMSE = RMSE(vg$mu_movie_user_time_genres, vg$rating)))
## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.5. RMSE by method V",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

##### 3.2 Model performance #####

## Update validation with timestamp converted to date format
validation <- validation %>% 
  mutate(rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01 00:00:00",tz = "GMT"))) %>% 
  mutate(rating_year = year(rating_time)) %>%
  select(-timestamp)

## Calculate the weeks elapsed
validation <- validation %>% left_join(first_sum, by = "movieId")
validation <- validation %>%
  mutate(weeks_elapsed = as.numeric(round((rating_time - first_rating_time)/7,0)))

## Append effects to validation
validation <- validation %>% left_join(b_u_sum, by = "userId") %>%
  left_join(b_t_sum, by = "weeks_elapsed") %>%
  left_join(b_g_sum, by = "genres")%>%
  left_join(movie_sum, by = "movieId")

## Check for NAs in validation set by displaying a table
knitr::kable(data.frame(NAs = colSums(is.na(validation[,12:14]))),
             caption = "Table 3.6. Check for NA values in validation set") %>% 
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Convert NAs to mean of time effect in validation
validation$b_t[is.na(validation$b_t)] <- mean(validation$b_t, na.rm = T)

## Combine effects for final predicted ratings
validation <- validation %>%
  mutate(predicted_rating = mu_movie + b_u + b_t + b_g)

## Create and display the final RMSE table
data_frame(method = "Movie, user, time, genre effect model",
           RMSE = RMSE(validation$rating, validation$predicted_rating)) %>%
  knitr::kable(caption = "Table 3.7. Final RMSE evaluation",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Print final RMSE
RMSE(validation$rating, validation$predicted_rating)
