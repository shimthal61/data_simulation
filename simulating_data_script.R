# 24 participants. PPs 1-1  2 are in the "fast" condition, PPs 13-24 in the "slow' condition

# Let's create a vector for our PP ID number, ranging from 1 to 24

participant <- seq(1:24) 
participant

# Let's generate our condition
condition <- c(rep("fast", times = 12), rep("slow", times = 12))
condition               

# Now, let's simulate some data using the rnorm() function

# We want to simulate our data as "fast" condition with mean = 1000, and sd = 50, and the data
# for our "slow" condtion from a distribution with a mean = 1020 and sd = 50

set.seed(42)
dv <- c(rnorm(12, 1000, 50), rnorm(12, 1020, 50))
dv

# Let's combine our 3 columnss (PP, condition, dv) into a tibble

my_data <- as_tibble(cbind(participant, condition, dv))
my_data

str(my_data)

# Our condition and dv factors are currently coded as character strings

my_tidied_data <- my_data %>% 
  mutate(condition = factor(condition),
         dv = as.integer(dv))

my_tidied_data

my_tidied_data %>% 
  ggplot(aes(x = condition, y = dv, fill = condition)) +
  geom_violin(width = .5) +
  geom_point(alpha = .5, position = position_jitter(width = .2, seed = 42)) +
  guides(fill = 'none') +
  theme_minimal() +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
  xlab("Condition")

my_tidied_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(dv), sd = sd(dv))

# We can perform an independent samples t-test to see if the conditions differ

t.test(filter(my_tidied_data, condition == "fast")$dv, filter(my_tidied_data, condition == "slow")$dv, paired = FALSE)

# our p-value is .072 - no statistical difference between our two conditions

# However, this output is a bit messy - let's extract the important stuff

result <- tidy(t.test(filter(my_tidied_data, condition == "fast")$dv, filter(my_tidied_data, condition == "slow")$dv, paired = FALSE))

result

#now we have all the key stats 

result$p.value
