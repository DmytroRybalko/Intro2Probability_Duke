## Calculate the total number of births for each year and store these values in
## a new variable called `total` in the `present` dataset.
present <- present %>%
    mutate(total = boys + girls)

## calculate the proportion of boys born each year and store these values in
## a new variable called `prop_boys` in the same dataset
present$prop_boys <- present$boys / present$total

## Plot these values over time and based on the plot determine if the following
## statement is true or false: The proportion of boys born in the US has 
## decreased over time.
ggplot(data = present, aes(x = year, y = prop_boys)) +
    geom_line() +
    geom_point()

# 6. Create a new variable called `more_boys` which contains the value of 
# either `TRUE` if that year had more boys than girls, or `FALSE` if that year
# did not. Based on this variable which of the following statements is true?

present <- present %>%
    mutate(more_boys = boys > girls)

## 7. Calculate the boy-to-girl ratio each year, and store these values in a
## new variable called `prop_boy_girl` in the `present` dataset. Plot these
## values over time. Which of the following best describes the trend? 

present <- present %>%
    mutate(prop_boy_girl = boys / girls)

ggplot(data = present, aes(x = year, y = prop_boy_girl)) +
    geom_line() +
    geom_point()