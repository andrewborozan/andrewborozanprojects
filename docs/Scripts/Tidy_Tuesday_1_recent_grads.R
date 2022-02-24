library(tidyverse)

recent_grads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-16/recent-grads.csv")

# Pick a couple of columns that are the most interesting
# and make a visualization of them.

# Salary
theme_set(theme_clean())
recent_grads %>% ggplot(aes(x = Median)) +
  geom_histogram()


#Is the outlier for real or due to low sample size? 

# How about median per major? (BOXPLOT = 1 categorical - x axis, 1 continuous - y axis)
recent_grads %>% mutate(Major_category = fct_reorder(Major_category, Median)) %>% 
  ggplot(aes(Major_category, Median)) +
  geom_boxplot()+
  scale_y_continuous(labels = dollar_format())+
  coord_flip()

# Did it without reordering first. Mutate reorder - reorder the major 
# category by median. 

# Show just the median in columns
recent_grads %>% group_by(Major_category) %>% summarize(Median = median(Median)) %>%
  mutate(Major_category = fct_reorder(Major_category, Median)) %>% 
  ggplot(aes(Major_category, Median)) + geom_col() + scale_y_continuous(labels = dollar_format()) +
  coord_flip()


#What categories of majors make more money than others?
# Make a TIE FIGHTER plot - interquartile range - median as dot in middle

recent_grads %>% 
  arrange(desc(Median)) %>% 
  select(Major, Major_category, Median, P25th, P75th) %>% 
  head(20) %>% 
  mutate(Major = str_to_title(Major), #str_to_title does PROPER case
         Major = fct_reorder(Major, Median)) %>% 
  ggplot(aes(Major, Median, color = Major_category)) +
  geom_point() +
  geom_errorbar(aes(ymin = P25th, ymax = P75th)) +
  coord_flip() +
  expand_limits(y = 0)


# What are the lowest earning majors?
recent_grads %>% 
  arrange(desc(Median)) %>% 
  select(Major, Major_category, Median, P25th, P75th) %>% 
  tail(20) %>% 
  mutate(Major = str_to_title(Major), #str_to_title does PROPER case
         Major = fct_reorder(Major, Median)) %>% 
  ggplot(aes(Major, Median, color = Major_category)) +
  geom_point() +
  geom_errorbar(aes(ymin = P25th, ymax = P75th)) +
  coord_flip() +
  expand_limits(y = 0)

# Sample size makes a difference
recent_grads %>% 
  arrange(desc(Median)) %>% 
  select(Major, Major_category, Median, P25th, P75th, Sample_size) %>% 
  head(20) 

# Some are only based on 3 observations

library(ggrepel)
recent_grads %>% 
  ggplot(aes(Sample_size, Median)) + 
  geom_point() +
  scale_x_log10()


# Most common majors category - if you didn't include the weight, you would just get 
# the count of how many times the major category shows up. Adding the weight here shows how many
# people per category
recent_grads %>% count(Major_category, wt = Total, sort = TRUE) %>% 
  mutate(Major_category = fct_reorder(Major_category, n)) %>% 
  ggplot(aes(Major_category, n, fill = Major_category)) + geom_col() + coord_flip() +
  scale_fill_viridis_d()+
  theme(legend.position = "none")
  
# Most popular majors by total
recent_grads %>% mutate(Major = fct_reorder(Major, Total)) %>% 
  arrange(desc(Total)) %>% 
  head(20)


recent_grads %>% mutate(Major = fct_reorder(Major, Total)) %>% 
  arrange(desc(Total)) %>% 
  head(20) %>% 
  gather(Gender, Number, Men, Women) %>% select(Major, Gender, Number) %>% 
  ggplot(aes(Major, Number, fill = Gender)) +
  geom_col()+
  coord_flip()

# Creates a ratio column for men and women to total per major
recent_grads <- recent_grads %>% 
  mutate(MenRatio = Men/Total, WomenRatio = Women/Total) %>% 
  relocate(MenRatio, .after = Men) %>% 
  relocate(WomenRatio, .after = Women)


# Top 20 highest men to women ratio for majors (put in labels on bars to make this better)
recent_grads %>% mutate(Major = fct_reorder(Major, MenRatio)) %>% 
  arrange(desc(MenRatio)) %>% 
  head(20) %>% 
  gather(Gender, Number, Men, Women) %>% select(Major, Gender, Number, MenRatio) %>% 
  ggplot(aes(Major, Number, fill = Gender)) +
  geom_col()+
  coord_flip()

# Top 20 highest women to men ratio for majors
recent_grads %>% mutate(Major = fct_reorder(Major, WomenRatio)) %>% 
  arrange(desc(WomenRatio)) %>% 
  head(20) %>% 
  gather(Gender, Number, Men, Women) %>% select(Major, Gender, Number, MenRatio) %>% 
  ggplot(aes(Major, Number, fill = Gender)) +
  geom_col()+
  coord_flip()

womensharesalary<- recent_grads %>% group_by(Major_category) %>%
  filter(!is.na(Total)) %>% 
  summarize(Men = sum(Men), 
            Women = sum(Women), 
            Total = sum(Total),
            MedianSalary = sum(Median * Sample_size)/ sum(Sample_size), 
            ) %>% 
  mutate(ShareWomen = Women/Total) %>% 
  arrange(desc(ShareWomen))


 womensharesalary %>% ggplot(aes(ShareWomen, MedianSalary))+
   geom_point()+
   geom_smooth(method = "lm") +
   geom_text_repel(aes(label = Major_category), force = .5)+
   expand_limits(y = 0)
  
 
# Statistical Tests
 
statgrad <- recent_grads %>% select(Major, Total, ShareWomen, Sample_size, Median)

# Predict Median from sharewomen and see if it is significant effect and we get to use sample size as weighting variable.
# It will be a weighted linear regression. Won't pay a lot of attention to the ones that have low sample sizes.

statgrad %>% lm(Median ~ ShareWomen, data = ., weights = Sample_size) %>% 
  summary()

# Dave said how to read this is that if the field went from 0% women to 100% the median salary would drop -23650.
# For every percentage from men to women (45% men to %44 men), you lose $236 (-23650 * .01)


