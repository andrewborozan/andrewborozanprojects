outcomes <- outcomes %>% clean_names()


# Here is a look at the frequency of each learning outcome is measured
outcomes %>% count(learning_outcomes_name, sort = TRUE)

# Here is a look at the frequency of each assessment type is measured
outcomes %>% count(outcome_assessments_type, sort = TRUE)

#How many students and how many assessments they have taken
outcomes %>% count(target_users_id, sort = TRUE) %>% view()
#147 students but 12 have taken 3 or fewer assessments.


outcomes %>% count(outcome_assessments_klass_id, sort = TRUE)

# There is a lack of robust data for a couple of columns so we will bypass them
# in our analysis. Assessment ids have many have NAs. There is also some unclarity (even in the data dictionary) as
# to how the assignment weights work. 
outcomes %>% count(outcome_assessments_assignment_id, sort = TRUE)
outcomes %>% count(assignments_weight, sort = TRUE)



# Line graph over time, avg. score on y-axis, line color by learning outcome type (for all students)
showtext.auto()

# All on one
outcomes %>% group_by(learning_outcomes_name, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
  ggplot(aes(outcome_assessments_created_date, avg_score, color = learning_outcomes_name)) +
  geom_line(size = 1.5)+  
  scale_color_viridis(name = "Learning Outcomes", option = "magma", discrete = TRUE, begin = .9, end = .1)+
  labs(x = "", y = "Average Score", title = "Average score over time by learning outcomes")+
  scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
  theme(text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 20)) 

# Faceted
outcomes %>% group_by(learning_outcomes_name, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
  ggplot(aes(outcome_assessments_created_date, avg_score, color = learning_outcomes_name)) +
  geom_line(size = 1.5, show.legend = FALSE)+  
  scale_color_viridis(name = "Learning Outcomes", option = "magma", discrete = TRUE, begin = .9, end = .1)+
  labs(x = "", y = "Average Score", title = "Average score over time by learning outcomes")+
  scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
  facet_wrap(~ learning_outcomes_name)+
  theme(text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 20)) 

# Same as above but can filter by individual students and see scores over time by outcome type
showtext.auto()

# All in graph
outcomes %>% filter(target_users_id == 15375) %>% 
  group_by(learning_outcomes_name, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
  ggplot(aes(outcome_assessments_created_date, avg_score, color = learning_outcomes_name)) +
  geom_line(size = 1.5)+  
  scale_color_viridis(name = "Learning Outcomes", option = "magma", discrete = TRUE, begin = .9, end = .1)+
  labs(x = "", y = "Average Score", title = "Average score over time by learning outcomes")+
  scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
  theme(text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 20))


# Faceted
outcomes %>% filter(target_users_id == 15375) %>% 
  group_by(learning_outcomes_name, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
  ggplot(aes(outcome_assessments_created_date, avg_score, color = learning_outcomes_name)) +
  geom_line(size = 1.5, show.legend = FALSE)+  
  scale_color_viridis(name = "Learning Outcomes", option = "magma", discrete = TRUE, begin = .9, end = .1)+
  labs(x = "", y = "Average Score", title = "Average score over time by learning outcomes")+
  scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
  facet_wrap(~ learning_outcomes_name)+
  theme(text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 20))


# Filter by student as function, so all you have to do is put in target_user_id
student_outcome_scores <- function(x){
  outcomes %>% filter(target_users_id == x) %>% 
    group_by(learning_outcomes_name, outcome_assessments_created_date) %>% 
    summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
    ggplot(aes(outcome_assessments_created_date, avg_score, color = learning_outcomes_name)) +
    geom_line(size = 1.5)+  
    scale_color_viridis(name = "Learning Outcomes", option = "magma", discrete = TRUE, begin = .9, end = .1)+
    labs(x = "", y = "Average Score", title = "Average score over time by learning outcomes")+
    scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
    theme(text = element_text(family = "exo", size = 20),
          plot.title = element_text(size = 20))
}

#Function in action
student_outcome_scores(15743)

# Faceted Function
student_outcome_scores_facet <- function(x){
  outcomes %>% filter(target_users_id == 15375) %>% 
    group_by(learning_outcomes_name, outcome_assessments_created_date) %>% 
    summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
    ggplot(aes(outcome_assessments_created_date, avg_score, color = learning_outcomes_name)) +
    geom_line(size = 1.5, show.legend = FALSE)+  
    scale_color_viridis(name = "Learning Outcomes", option = "magma", discrete = TRUE, begin = .9, end = .1)+
    labs(x = "", y = "Average Score", title = "Average score over time by learning outcomes")+
    scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
    facet_wrap(~ learning_outcomes_name)+
    theme(text = element_text(family = "exo", size = 20),
          plot.title = element_text(size = 20))
}

student_outcome_scores_facet(15743)

# Same thing but by assessment type
showtext.auto()
outcomes %>% group_by(outcome_assessments_type, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
  ggplot(aes(outcome_assessments_created_date, avg_score, color = outcome_assessments_type)) +
  geom_line(size = 1.5)+  
  scale_color_viridis(name = "Assessment Type", option = "magma", discrete = TRUE, begin = .9, end = .1)+
  labs(x = "", y = "Average Score", title = "Average score over time by assessment type")+
  scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
  theme(text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 20))

# Shows student scores over time by assessment type

outcomes %>% filter(target_users_id == 15375) %>%
  group_by(outcome_assessments_type, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
  ggplot(aes(outcome_assessments_created_date, avg_score, color = outcome_assessments_type)) +
  geom_line(size = 1.5)+  
  scale_color_viridis(name = "Assessment Type", option = "magma", discrete = TRUE, begin = .9, end = .1)+
  labs(x = "", y = "Average Score", title = "Average score over time by assessment type")+
  scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
  theme(text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 20))

# Filter by student as function, so all you have to do is put in target_user_id
student_assessmenttype_scores <- function(x){
  outcomes %>% filter(target_users_id == x) %>%
    group_by(outcome_assessments_type, outcome_assessments_created_date) %>% 
    summarize(avg_score = mean(outcome_assessments_score, na.rm = TRUE)) %>% 
    ggplot(aes(outcome_assessments_created_date, avg_score, color = outcome_assessments_type)) +
    geom_line(size = 1.5)+  
    scale_color_viridis(name = "Assessment Type", option = "magma", discrete = TRUE, begin = .9, end = .1)+
    labs(x = "", y = "Average Score", title = "Average score over time by assessment type")+
    scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0, 4.5))+
    theme(text = element_text(family = "exo", size = 20),
          plot.title = element_text(size = 20))
}

# Function in action
student_assessmenttype_scores(15743)

# boxplots showing range, quartiles, and median of scores by assessment type for each outcome
outcomes %>% 
  mutate(learning_outcomes_name = fct_reorder(learning_outcomes_name, outcome_assessments_score, na.rm = TRUE),
         learning_outcomes_name = fct_lump_n(learning_outcomes_name, n = 7)) %>% 
  ggplot(aes(outcome_assessments_type, outcome_assessments_score, fill = outcome_assessments_type)) +
  geom_boxplot(show.legend = FALSE)+
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = .9, end = .3)+
  facet_wrap(~ learning_outcomes_name)

# The boxplots above but as a ridgeline - in analysis point out peaks
outcomes %>% 
  mutate(learning_outcomes_name = fct_reorder(learning_outcomes_name, outcome_assessments_score, na.rm = TRUE),
         learning_outcomes_name = fct_lump_n(learning_outcomes_name, n = 7)) %>% 
  ggplot(aes(outcome_assessments_score, outcome_assessments_type, fill = outcome_assessments_type)) +
  geom_density_ridges(show.legend = FALSE)+ 
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = .9, end = .3)+
  facet_wrap(~ learning_outcomes_name, scales = "free")+
  labs(x = "Scores", y = "", title = "Scores by outcome and assessment type")
  theme(panel.spacing = unit(2.5, "lines"),
        axis.line=element_line(), 
        text = element_text(family = "exo", size = 20),
        plot.title = element_text(size = 27))



#Plotly graphs of each assessment by type and outcome over time
scores_by_assesstype <- outcomes %>% drop_na(outcome_assessments_score) %>% 
ggplot(aes(outcome_assessments_created_date, outcome_assessments_score, 
           label = target_users_id)) + # The "label = movie"
  # here will make it so that you can get the name of the movie in the interactive graph later
  geom_point() +
  geom_jitter(aes(color = learning_outcomes_name))+
  geom_smooth(method = "lm", color = "black") +
  labs(x = "", y = "Assessment Score", title = "Scores over time by type and outcome")+
  scale_color_viridis(name = "Learning Outcome", option = "magma", discrete = TRUE, begin = .95, end = .2)+
  facet_wrap(~ outcome_assessments_type, scales = "free")+
  theme(panel.spacing = unit(1.5, "lines"),
        axis.line=element_line(), 
        text = element_text(family = "exo", size = 15))

ggplotly(scores_by_assesstype)


#searchable table of average scores over each month
student_avg_by_month <- outcomes %>% drop_na(outcome_assessments_score) %>% 
  group_by(target_users_id, outcome_assessments_created_date) %>% 
  summarize(avg_score = mean(outcome_assessments_score)) %>% 
  spread(outcome_assessments_created_date, avg_score) %>%
  rename("September 2021" = "2021-09-01", 
         "October 2021" = "2021-10-01", 
         "November 2021" = "2021-11-01", 
         "December 2021" = "2021-12-01") %>% 
  select(-"2022-01-01")

reactable(student_avg_by_month, wrap = TRUE, theme = fivethirtyeight(), striped = TRUE, 
          fullWidth = TRUE, searchable = TRUE, sortable = TRUE, filterable = TRUE)





outcomes %>% filter(outcome_assessments_type == "assignment", outcome_assessments_created_date < 2021-10-01) %>% 
  view()

lm(outcomes$outcome_assessments_type ~ outcomes$outcome_assessments_created_date)





