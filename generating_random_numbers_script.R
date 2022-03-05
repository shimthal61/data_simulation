# We can generate random numbers using the rnorm() function. We specify how many
# samples we want to generate, the mean, and the standard deviation

rnorm(3, 0, 1)

rnorm(3, 0, 1)

# R generates numbers pseudo-randomly using an algorithm. We can set the seed
# to ensure reproducibility. 

set.seed(42)
rnorm(3, 0, 1)

set.seed(42)
rnorm(3, 0, 1)

# If we set the seed before each rnorm() call, then we always get the same output

set.seed(42)
my_data <- rnorm(50, 0, 1)
hist(my_data)

#This looks something like the normal distribution

# If we increase our sample size, it looks more like normal dist

set.seed(42)
my_data <- rnorm(5000000, 0, 1)
hist(my_data)

# We can calculate how many data points are more than 1.96 sds from the mean

my_data %>% 
  as_tibble() %>% 
  filter(value > 1.96 | value < -1.96) %>% 
  count()

(250437/5000000) * 100

# 5.00874% of our data points are more than 1.96 sds either side of the mean

# Let's plot our simulated data from two distributions

# The cbind() function means column bind, which we've converted to a tibble

set.seed(42)
condition1 <- rnorm(1000000, 0, 1)
condition2 <- rnorm(1000000, 1.96, 1)
my_data <- as_tibble(cbind(condition1, condition2))

ggplot(my_data) +
  geom_density(aes(x = condition1, y = ..density.., colour = "red")) +
  geom_density(aes(x = condition2, y = ..density.., colour = "green")) +
  xlab("Data") +
  guides(colour = 'none')

# We can use the function c() to combine elements into one vector

a <- c(1, 2, 3)
b <- c(4, 5, 6)
cbind(a,b)

# We can do the same with rbind() - row bind

a <- c(1, 2, 3)
b <- c(4, 5, 6)
rbind(a,b)

# We can also sequence data using seq()

seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, by = 2)

# We can replicate elements in a vector a certain number of times using rep()

rep(1:5, times = 2)

# We can combine rep() and seq()

rep(seq(from = 1, to = 10, by = 2), times = 2)

# The vector doesn't need to be numbers

rep("fast", times = 12)

# We can use the combine function c() within the rep() function

c(rep("fast", times = 12), rep("slow", times = 12))
