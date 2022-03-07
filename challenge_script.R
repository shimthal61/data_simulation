total_samples <- 2000
sample_size <- 55
participant <- rep(1:sample_size)
condition <- c(rep("fast", times = sample_size/2),
               rep("slow", times = sample_size/2))
all_data <- NULL

for (i in 1:total_samples) {
  sample <- i
  set.seed(1233 + i)
  dv <- c(rnorm(sample_size/2, 1000, 50), rnorm(sample_size/2, 1040, 50))
  my_data <- as_tibble(cbind(participant, condition, dv, sample))
  all_data <- rbind(my_data, all_data)
}

all_tidied_data <- all_data %>%
  mutate(condition = factor(condition), dv = as.integer(dv))

result <- NULL
for (i in 1:total_samples) {
  result <- rbind(tidy(t.test(filter(all_tidied_data, 
                                     condition == "fast" & sample == i)$dv,
                              filter(all_tidied_data, 
                                     condition == "slow" & sample == i)$dv,
                              paired = FALSE)), result)
}

result

(p_values <- result %>% 
  filter(p.value < .05) %>% 
  count())

#Cohen's d value:
(100/(total_samples/p_values))/100

# We need a cohen's d value of 0.8 - 80% of the studies to be positive 
(1040-1000)/50

# 40/200 were sig

# If we double the sd in both conditions, 59/200 were significant

# We need ~55 participants to find an effect size of 0.8
my_tidied_data

model <- aov_4(dv~condition + (1 | participant), data = my_tidied_data)

summary(model)

# p = .07. No statistical difference between the two conditions