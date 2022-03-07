total_samples <- 100
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

all_tidied_data %>%
  group_by(condition, sample) %>%
  summarise(average = mean(dv)) %>%
  ggplot(aes(x = condition, y = average, group = condition,
             label = sample)) +
  geom_jitter(width = .1, alpha = .5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "blue") +
  geom_text(check_overlap = TRUE, nudge_x = .2, nudge_y = 0, colour =
              "black", size = 3) +
  labs(x = "Condition", y = "DV(ms.)") +
  theme_minimal()

#running t-tests
result <- NULL
for (i in 1:total_samples) {
  result <- rbind(tidy(t.test(filter(all_tidied_data, 
                                     condition == "fast" & sample == i)$dv,
                              filter(all_tidied_data, 
                                     condition == "slow" & sample == i)$dv,
                              paired = FALSE)), result)
}

result

# Let's see how many were significant

result %>% 
  filter(p.value < .05) %>%
  count()

# This means that 21 times out of 100, we find a sig difference - even though
# we know an effect exists in the population

# Out study is very under powered. Let's work out cohen's d for effect size

(1020-1000)/50