# Exercise 4: practicing with dplyr

# Install the `"nycflights13"` package. Load (`library()`) the package.
# You'll also need to load `dplyr`

library(nycflights13)
library(dplyr)

library(data.table)
# The data frame `flights` should now be accessible to you.
# Use functions to inspect it: how many rows and columns does it have?
# What are the names of the columns?
# Use `??flights` to search for documentation on the data set (for what the 
# columns represent)

??flights
names(flights)
data(flights)

# Use `dplyr` to give the data frame a new column that is the amount of time
# gained or lost while flying (that is: how much of the delay arriving occured
# during flight, as opposed to before departing).
flights %>% mutate(gain = 1)

flights <- mutate(flights, gain_in_air = arr_delay - dep_delay)

dt <- as.data.table(flights)



# Use `dplyr` to sort your data frame in descending order by the column you just
# created. Remember to save this as a variable (or in the same one!)

flights <- arrange(flights, desc(gain_in_air))
View(flights)

dt = dt[order(gain_in_air, decreasing = T)]


# For practice, repeat the last 2 steps in a single statement using the pipe
# operator. You can clear your environmental variables to "reset" the data frame
flights <- flights %>% mutate(gain_in_air = arr_delay - dep_delay) %>% arrange(desc(gain_in_air))


dt <- dt[,gain_in_air:=arr_delay - dep_delay][order(gain_in_air, decreasing = T)]

# Make a histogram of the amount of time gained using the `hist()` function
hist(dt[,gain_in_air])
hist(flights$gain_in_air)

# On average, did flights gain or lose time?
# Note: use the `na.rm = TRUE` argument to remove NA values from your aggregation
dt[,mean(gain_in_air, na.rm = T)]
flights$gain_in_air %>% mean(na.rm=T)


# Create a data.frame of flights headed to SeaTac ('SEA'), only including the
# origin, destination, and the "gain_in_air" column you just created
dtSEA = dt[dest=='SEA',.(origin, dest, gain_in_air)]

to_sea <- flights %>% select(origin, dest, gain_in_air) %>% filter(dest == "SEA")

# On average, did flights to SeaTac gain or loose time?
mean(to_sea$gain_in_air, na.rm = T)



# Consider flights from JFK to SEA. What was the average, min, and max air time
# of those flights? Bonus: use pipes to answer this question in one statement
# (without showing any other data)!

filter(flights, origin == "JFK", dest == "SEA") %>%
  summarize(
    avg_air_time = mean(air_time, na.rm = TRUE),
    max_air_time = max(air_time, na.rm = TRUE),
    min_air_time = min(air_time, na.rm = TRUE)
  )

dt[origin=='JFK' & dest=='SEA',
   .(mean(air_time, na.rm = T),
    min(air_time, na.rm = T),
    max(air_time, na.rm = T))]

