# Script to clean up mturk data and make it long

library(knitr)
library(stringr)
library(magrittr)
library(gridExtra)
library(kableExtra)
library(formattable)
library(ggplot2)
library(dplyr)
library(jtools)
library(kableExtra) # if you are generating your own RMarkdown documents and have kableExtra installed, you’ll instead get some prettier looking tables
library(ggfortify)
library(fixest)
library(tidyr)
library(RColorBrewer)
library(data.table)
#'
#'
#+ echo = FALSE, message = FALSE, warning = FALSE

mturk <- read.csv(file = "~/jobsearchmturk/Data/MTurk_6_WideCSV_1.csv", header = TRUE, stringsAsFactors = FALSE)
df <- mturk %>% 
  select(WorkerId, c(28:41), Answer.rankings_rev)

# Wage
wage <- df %>% 
  select(WorkerId, Input.choice_id, Input.job_wage_A, Input.job_wage_B, Input.job_wage_C, Answer.applicationInfoShown, Answer.rankings_rev) %>% 
  pivot_longer(cols = c("Input.job_wage_A", "Input.job_wage_B", "Input.job_wage_C"), names_to = "job", values_to = "wage") %>% 
  mutate(job = case_when(
    job == "Input.job_wage_A" ~ "A",
    job == "Input.job_wage_B" ~ "B",
    job == "Input.job_wage_C" ~ "C",
    TRUE ~ NA_character_
  ))

# Commute
commute <- df %>% 
  select(WorkerId, Input.commute_time_A, Input.commute_time_B, Input.commute_time_C) %>% 
  pivot_longer(cols = c("Input.commute_time_A", "Input.commute_time_B", "Input.commute_time_C"), names_to = "job", values_to = "commute_time") 

# Applications
applications <- df %>% 
  select(WorkerId, Input.applications_A, Input.applications_B, Input.applications_C) %>% 
  pivot_longer(cols = c("Input.applications_A", "Input.applications_B", "Input.applications_C"), names_to = "job", values_to = "applications")

# Color
color <- df %>% 
  select(WorkerId, Input.color_A, Input.color_B, Input.color_C) %>% 
  pivot_longer(cols = c("Input.color_A", "Input.color_B", "Input.color_C"), names_to = "job", values_to = "color")

# Combine tibbles
df <- wage %>% 
  bind_cols(commute[, "commute_time"], applications[, "applications"], color[, "color"])

# Check
head(df)

df %<>%
  separate(Answer.rankings_rev, into = c("rank1", "rank2", "rank3"), sep = ",") %>%
  mutate(rank1 = str_replace_all(rank1, pattern = "\\s+|[[:punct:]]+", replacement = ""), # Remove symbols/punctuation
         rank2 = str_replace_all(rank2, pattern = "\\s+|[[:punct:]]+", replacement = ""),
         rank3 = str_replace_all(rank3, pattern = "\\s+|[[:punct:]]+", replacement = ""),
         ranking = case_when(
           job == "A" & (rank1 == "rank1jobA" | rank2 == "rank1jobA" | rank3 == "rank1jobA") ~ 1L,
           job == "A" & (rank1 == "rank2jobA" | rank2 == "rank2jobA" | rank3 == "rank2jobA") ~ 2L,
           job == "A" & (rank1 == "rank3jobA" | rank2 == "rank3jobA" | rank3 == "rank3jobA") ~ 3L,
           job == "B" & (rank1 == "rank1jobB" | rank2 == "rank1jobB" | rank3 == "rank1jobB") ~ 1L,
           job == "B" & (rank1 == "rank2jobB" | rank2 == "rank2jobB" | rank3 == "rank2jobB") ~ 2L,
           job == "B" & (rank1 == "rank3jobB" | rank2 == "rank3jobB" | rank3 == "rank3jobB") ~ 3L,
           job == "C" & (rank1 == "rank1jobC" | rank2 == "rank1jobC" | rank3 == "rank1jobC") ~ 1L,
           job == "C" & (rank1 == "rank2jobC" | rank2 == "rank2jobC" | rank3 == "rank2jobC") ~ 2L,
           job == "C" & (rank1 == "rank3jobC" | rank2 == "rank3jobC" | rank3 == "rank3jobC") ~ 3L,
           TRUE ~ NA_integer_
         ),
         ranking = factor(ranking, levels = c(1,2,3), labels = c("First", "Second", "Third"))) %>% 
  relocate(ranking, .before = job)

df %<>% 
  mutate(rank_score = case_when(
    ranking == "First" ~ 3L,
    ranking == "Second" ~ 2L,
    ranking == "Third" ~ 1L,
    TRUE ~ NA_integer_
  ))


str(df$applications) # Character
unique(df$applications)
# [1] "Be one of the first to apply" "51-200 applications"          "5-20 applications"            "21 - 50 applications"         "200+ applications"           
# [6] "0-4 applications" 

# Convert applications variable to factor variable 
df %<>% 
  mutate(applications = factor(applications))

df_final <- df %>% 
  select(!c(rank1, rank2, rank3)) %>% 
  rename(question = Input.choice_id, # Rename variables
         info_shown = Answer.applicationInfoShown) %>% 
  mutate_if(is.character, as.factor) %>% # Convert any character variables to factor variables
  relocate(info_shown, .after = applications) %>% 
  relocate(job, .before = ranking)
setDT(df_final)
