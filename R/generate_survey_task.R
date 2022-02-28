# Code to generate job choice tasks for survey:
library(data.table)
library(here)
list_job_names <- c("A", "B", "C")
list_job_wage <- c(12, 15, 20, 25)
list_commute_time <- c(10, 20, 30, 45, 60)
list_applicants <- c("Be one of the first to apply", 
                '5-20 applications', '21 - 50 applications', '51-200 applications',
                '200+ applications')
list_color <- c("blue", "grey")

num_tasks <- 20
num_jobs <- 3
set.seed(3420)
data_choices <- data.table(choice_id = numeric(),
                           job_name = character(),
                           job_wage = numeric(),
                           commute_time = numeric(),
                           applications = character(),
                           color = character())
for(i in 1:num_tasks){
  # Represent be one of the first or 0 - 4. Be sure this is consistent per choice
  string_for_few <- sample(c('0-4 applications', "Be one of the first to apply"), 1)
  for(j in 1:num_jobs){
    this_data_choices <- data.table()
    this_data_choices[, choice_id := i]
    this_data_choices[, job_name := list_job_names[j]]
    this_data_choices[, job_wage := sample(list_job_wage, 1)]
    this_data_choices[, commute_time := sample(list_commute_time, 1)]
    this_data_choices[, applications := sample(list_applicants, 1)]
    this_data_choices[, color := sample(list_color, 1)]
    this_data_choices[applications == "Be one of the first to apply", 
                      applications := string_for_few]
    data_choices <- rbind(data_choices, this_data_choices)
  }
}
data_choices_wide <- dcast.data.table(data_choices, choice_id ~ job_name, value.var = c("job_wage", "commute_time", "applications", "color"))
fwrite(data_choices_wide, file = here('lab-nb-spring2022/HITs/choices_survey_wide.csv'))

       