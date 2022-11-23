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

## Display the vg summary table
vg_summary %>% 
  knitr::kable(caption = "Table 1.2. Summary of vg set") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Display rows in vg with no genre information
vg %>%
  filter(Genre == "") %>% 
  knitr::kable(caption = "Table 1.3. Rows in vg lacking genre information",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

##### 2.0 Methods and analysis #####

## Create and display summary table of user scores
describe(vg$User_Score, fast = TRUE) %>%
  select(-vars) %>%
  knitr::kable(caption = "Table 2.1. Summary statistics for user scores",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Create a table with the sum of each user score
u_s_sum <- vg %>% filter(User_Score != mean(User_Score)) %>% group_by(User_Score) %>%
  summarize(count = n())

which.max(u_s_sum$count)
u_s_sum[76,]

## Plot the count by user score using the user score sum table
u_s_sum %>%
  ggplot(aes(User_Score, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "User score", y = "Count")

## Create and display scatterplot of average and number of user scores
vg %>% 
  ggplot(aes(User_Count, User_Score)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) +
  labs(x = "Number of user scores",
       y = "Average score")

##### 2.1 Techniques and processes #####

## Update vg with user score multiplied by 10 to scale to critic score
vg <- vg %>% 
  mutate(User_Score = User_Score*10)

## View first five rows of updated vg dataset
vg %>% slice(1:5) %>% knitr::kable(caption = "Table 2.2. First five rows of updated vg dataset",
                                    row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down","HOLD_position"))

## Create and display summary table of critic scores
describe(vg$Critic_Score, fast = TRUE) %>%
  select(-vars) %>%
  knitr::kable(caption = "Table 2.2. Summary statistics for critic scores",
               row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Create a table with the sum of each critic score
c_s_sum <- vg %>% filter(Critic_Score != mean(Critic_Score)) %>% group_by(Critic_Score) %>%
  summarize(count = n())

## Plot the count by critic score using the critic score sum table
c_s_sum %>%
  ggplot(aes(Critic_Score, count)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) + 
  labs(x = "Critic score", y = "Count")

## Create and display scatterplot of average and number of critic scores
vg %>% 
  ggplot(aes(Critic_Count, Critic_Score)) +
  geom_point(color = "steel blue", alpha = 0.3) +
  geom_smooth() +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25)) +
  labs(x = "Number of critic scores",
       y = "Average score")

## create quick summary table for plot of the number of user scores by year of release
year_sum <- vg %>% filter(Year_of_Release != "N/A") %>% group_by(Year_of_Release) %>%
  summarize(n = n(), Average_user_score = mean(User_Score))

## Display plot of number of user scores by year of release
ggplot(year_sum, aes(Year_of_Release, n)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  labs(x = "Year",
       y = "Count") + 
  theme(axis.text.x=element_text(angle = 80, hjust = 1),
        plot.background = element_rect(color = "black", fill=NA, size=0.25))

year_sum

## Plot the average user score by year of release
ggplot(year_sum, aes(as.numeric(Year_of_Release), Average_user_score)) +
  geom_point(color = "steel blue", alpha = 0.6) +
  geom_smooth(color = "steel blue") +
  theme_classic() +
  labs(x = "Year",
       y = "Rating") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Fit linear model of average rating on year of release and year of release squared
fit_lm1 <- lm(Average_user_score ~ I(as.numeric(Year_of_Release)^2) +
                I(as.numeric(Year_of_Release)), 
              data = year_sum)

## Fit linear model of average rating on year of release, year of release squared, and year of release cubed
fit_lm2 <- lm(Average_user_score ~ I(as.numeric(Year_of_Release)^3) +
                I(as.numeric(Year_of_Release)^2) +
                I(as.numeric(Year_of_Release)), 
              data = year_sum)

## Create table of results for both linear models
export_summs(fit_lm1, fit_lm2)

## Create and display a table describing the genres in vg
genres_sum <- vg %>% filter(Genre != "") %>% group_by(Genre) %>%
  summarize(Count = n(),
            User_mean = mean(User_Score),
            Critic_mean = mean(Critic_Score))
genres_sum %>%
  knitr::kable(caption = "Table 2.7. Statistics by genres in vg",
             digits = 2,
             row.names = FALSE) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("HOLD_position"))

## Plot average user score by genre
genres_sum %>%
  ggplot(
    aes(x = reorder(Genre, User_mean), User_mean)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  coord_flip() +
  labs(
    y = "Average user score",
    x = "Genres") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

## Create list of games close to the average
game_list <- vg %>% 
  filter(User_Score != mean(User_Score),
         User_Score >= round(mean(User_Score),2)-1,
         User_Score <= round(mean(User_Score),2)+1) %>% 
  select(Name, Genre, User_Score)

## Sample 10 games at random
set.seed(1, sample.kind = "Rounding")
game_list <- sample(game_list$Name, 10)

## Create average user score figure by genre for the 10 random games
vg_random <- vg %>%
  filter(Name %in% game_list)

random_genres <- vg_random %>% group_by(Genre) %>%
  summarize(Count = n(),
            User_mean = mean(User_Score),
            Critic_mean = mean(Critic_Score))

## Plot average user score by genre for random sample of 10 users
random_genres %>%
  ggplot(
    aes(x = reorder(Genre, User_mean), User_mean)) +
  geom_col(fill = "steel blue", color = "black") +
  theme_classic() +
  coord_flip() +
  labs(
    y = "Average user score",
    x = "Genres") + 
  theme(plot.background = element_rect(color = "black", fill=NA, size=0.25))

##### 3.1 Model results #####

## Compute RMSE using the average of all user scores
vg <- vg %>% mutate(mu_user = mean(User_Score))

rmses <- data_frame(method = "Only estimate is the average",
                    RMSE = RMSE(vg$mu_user, vg$User_Score))

## Create the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.1. RMSE by method I",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add critic--average effect to vg
b_c_sum <- vg %>% mutate(yhat = User_Score - mu_user) %>%
  group_by(Critic_Score) %>%
  summarize(b_c = mean(yhat))

vg <- left_join(vg, b_c_sum, by = "Critic_Score") %>%
  mutate(mu_c = mu_user + b_c)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add critic–specific effect",
                              RMSE = RMSE(vg$mu_c, vg$User_Score)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.2. RMSE by method II",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add platform effect to vg
b_p_sum <- vg %>% mutate(error = User_Score - mu_c) %>%
  group_by(Platform) %>%
  summarize(b_p = mean(error))

vg <- left_join(vg, b_p_sum, by = "Platform") %>%
  mutate(mu_c_p = mu_c + b_p)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add platform effect",
                              RMSE = RMSE(vg$mu_c_p, vg$User_Score)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.3. RMSE by method III",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add year effect to vg
b_y_sum <- vg %>% mutate(error = mu_c - mu_c_p) %>%
  group_by(Year_of_Release) %>%
  summarize(b_y = mean(error))

vg <- left_join(vg, b_y_sum, by = "Year_of_Release") %>%
  mutate(mu_c_p_y = mu_c_p + b_y)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add year effect",
                              RMSE = RMSE(vg$mu_c_p_y, vg$User_Score)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.4. RMSE by method IV",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add sales effect to vg
b_s_sum <- vg %>% mutate(error = User_Score - mu_c_p_y) %>%
  group_by(Global_Sales) %>%
  summarize(b_s = mean(error))

vg <- left_join(vg, b_s_sum, by = "Global_Sales") %>%
  mutate(mu_c_p_y_s = mu_c_p_y + b_s)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add sales effect",
                              RMSE = RMSE(vg$mu_c_p_y_s, vg$User_Score)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.5. RMSE by method V",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Add publisher effect to vg
b_pub_sum <- vg %>% mutate(error = User_Score - mu_c_p_y_s) %>%
  group_by(Publisher) %>%
  summarize(b_pub = mean(error))

vg <- left_join(vg, b_pub_sum, by = "Publisher") %>%
  mutate(mu_c_p_y_s_pub = mu_c_p_y_s + b_pub)

## Append the row to the RMSE results table
rmses <- bind_rows(rmses,
                   data_frame(method = "Add publisher--effect",
                              RMSE = RMSE(vg$mu_c_p_y_s_pub, vg$User_Score)))

## Display the RMSE results table
rmses %>%
  knitr::kable(caption = "Table 3.6. RMSE by method VI",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

##### 3.2 Model performance #####

## Make User_Score numeric in validation
validation <- transform(validation, User_Score = as.numeric(User_Score))

## Convert User_Score NAs to mean of User_Score in validation
validation$User_Score[is.na(validation$User_Score)] <- mean(validation$User_Score, na.rm = T)

## Update vg with user score multiplied by 10 to scale to critic score
validation <- validation %>% 
  mutate(User_Score = User_Score*10)

## Add mu_user to validation
validation <- validation %>% mutate(mu_user = rep(vg$mu_user[1],times = nrow(validation)))

## Append effects to validation
validation <- validation %>%
  left_join(b_c_sum, by = "Critic_Score") %>%
  left_join(b_p_sum, by = "Platform") %>%
  left_join(b_y_sum, by = "Year_of_Release") %>%
  left_join(b_s_sum, by = "Global_Sales") %>%
  left_join(b_pub_sum, by = "Publisher")

## Check for NAs in validation set by displaying a table
knitr::kable(data.frame(NAs = colSums(is.na(validation[,17:21]))),
             caption = "Table 3.7. Check for NA values in validation set") %>% 
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Convert NAs to mean of corresponding effects in validation
validation$b_c[is.na(validation$b_c)] <- mean(validation$b_c, na.rm = T)
validation$b_s[is.na(validation$b_s)] <- mean(validation$b_s, na.rm = T)
validation$b_pub[is.na(validation$b_pub)] <- mean(validation$b_pub, na.rm = T)

## Combine effects for final predicted ratings
validation <- validation %>%
  mutate(predicted_rating = mu_user + b_c + b_p + b_s + b_pub)

## Create and display the final RMSE table
data_frame(method = "Critic-average, platform, sales, publisher model",
           RMSE = RMSE(validation$User_Score, validation$predicted_rating)) %>%
  knitr::kable(caption = "Table 3.8. Final RMSE evaluation",
               row.names = FALSE,
               digits = 4) %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = "HOLD_position")

## Print final RMSE
RMSE(validation$User_Score, validation$predicted_rating)
