load("selected_nzes2011.Rdata")
library(dplyr)
selected_nzes2011 %>% 
    select(jpartyvote, jdiffvoting, singlefav) %>% 
    str()
names(selected_nzes2011)
## We can search the names for a fragment of the name by using the
## `grep("FRAGMENT", variable, value = TRUE)` command
grep("singlefav", names(selected_nzes2011), value = TRUE)
## Fix variable name
my_data <- selected_nzes2011 %>% 
    select(jpartyvote, jdiffvoting, X_singlefav)
str(my_data)

my_data %>% 
    group_by(jpartyvote) %>% 
    summarise(count = n())

my_data %>% 
    filter(jpartyvote != "Don't know") %>%
    group_by(jpartyvote) %>% 
    summarise(count = n())

my_data %>% 
    group_by(X_singlefav) %>% 
    summarise(count = n())

my_data %>% 
    filter(!is.na(X_singlefav)) %>%
    group_by(X_singlefav) %>% 
    summarise(count = n())

my_data %>%
    group_by(jdiffvoting) %>% 
    summarise(count = n())
## Check if for a given observation the values in the `jpartyvote` and
## `X_singlefav` variables are the same, or different:
my_data <- my_data %>%
    mutate(sameparty = ifelse(jpartyvote == X_singlefav, "same", "different"))

my_data %>% 
    group_by(jpartyvote, X_singlefav, sameparty) %>%
    summarise(count = n())

## To view and summarize the "same" entries we can use the following:
my_data %>% 
    group_by(jpartyvote, X_singlefav, sameparty) %>%
    summarise(count = n()) %>% 
    filter(sameparty == "same")

my_data %>% 
    group_by(jpartyvote, X_singlefav, sameparty) %>%
    summarise(count = n()) %>% 
    filter(sameparty == "different")

my_data %>% 
    group_by(jpartyvote, X_singlefav, sameparty) %>%
    summarise(count = n()) %>% 
    filter(is.na(sameparty))

## Step four. Prepare for the second question
# As a second question, we might be interested in exploring the relationship
# between age of voters and how much they like the NZ First party. We become
# familiar with the variables `jnzflike` and `jage` in the codebook, then
# explore the data.
str(selected_nzes2011$jnzflike)
str(selected_nzes2011$jage)

my_data2 <- selected_nzes2011 %>%
    select(jnzflike, jage)

str(my_data2)

my_data2 %>% 
    group_by(jnzflike) %>% 
    summarise(count = n())

my_data2 %>% 
    summarise(agemean = mean(jage),
              agemedian = median(jage),
              agesd = sd(jage), 
              agemin = min(jage),
              agemax = max(jage))

my_data2 %>% 
    filter(!(is.na(jage))) %>%
    summarise(agemean = mean(jage),
              agemedian = median(jage),
              agesd = sd(jage),
              agemin = min(jage),
              agemax = max(jage))

### Approach 1: Strongly liking and disliking NZ First and age
# If we wanted to select only two of the possible levels in how much people
# like NZ First, we can filter for these specific levels. When interested in
# filtering for multiple values a variable can take, the `%in%` operator can
# come in handy:
my_data2 %>% 
    filter(jnzflike %in% c("0","10")) %>%
    group_by(jnzflike) %>% 
    summarise(count = n())

### Approach 2: Age and liking for NZ First
# We might also like to refine our question slightly, asking do people above
# retirement age (65 in New Zealand) like NZ First more than younger people.
# To do this we can turn the numeric age variable into a categorical variable
# based on whether people are 65 years or older or younger than 65.
my_data2 <- my_data2 %>% 
    mutate(retiredage = ifelse(jage >= 65, "retired age", "working age"))

my_data2 %>% 
    group_by(retiredage) %>% 
    summarise(count = n())

my_data2 %>% 
    group_by(retiredage) %>% 
    summarise(medlike = median(jnzflike))

## Wromg way!!!
my_data2 <- my_data2 %>% 
    mutate(numlikenzf = as.numeric(jnzflike))
my_data2 %>% 
    group_by(jnzflike, numlikenzf) %>% 
    summarise(count = n())

## A conversion method that will use the text strings that label the levels,
## as opposed to the storage order of these levels. We can do this by first
## saving the variable as a character variable, and then turning it into a
## number
my_data2 <- my_data2 %>%
    mutate(numlikenzf = as.numeric(as.character(jnzflike)))

my_data2 %>% 
    group_by(jnzflike, numlikenzf) %>% 
    summarise(count = n())
