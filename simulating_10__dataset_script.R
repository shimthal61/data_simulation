# 'For' loops repeasr a command or a set of commands a certain number of times

# this doesn't work??
for (i in 1:10) {
  print(i)
}

my_tidied_data

for (i in 1:10) {
  print(my_tidied_data$dv)[i]
}

total_samples <- 10
sample_size <- 24
participant <- rep(1:sample_size)
condition <- c(rep("fast", times = sample_size/2),
               rep("slow", times = sample_size/2))
all_data <- NULL

for (i in 1:total_samples) {
  sample <- i
  set.seed(42 + i)
  dv <- c(rnorm(sample_size/2, 1000, 50), rnorm(sample_size/2, 1020, 50))
  my_data <- as_tibble(cbind(participant, condition, dv, sample))
  all_data <- rbind(my_data, all_data)
}

all_tidied_data <- all_data %>%
  mutate(condition = factor(condition), dv = as.integer(dv))

# The averages for all the experiments
all_tidied_data %>%
  group_by(condition, sample) %>%
  summarise(average = mean(dv)) %>%
  ggplot(aes(x = condition, y = average, group = condition,
             label = sample)) +
  geom_jitter(width = .1, alpha = .5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "blue") +
  geom_text(check_overlap = TRUE, nudge_x = .2, nudge_y = 0, colour =
              "black") +
  labs(x = "Condition", y = "DV(ms.)") +
  theme_minimal()

# The effect does not seem to be there for all the experiments. The effect is also
# polarised in some of the experiments. There is some sampling error

