### Day 1 Part 2 ###

## Data Load ##

# Read in the dataset
gapminder <- read.csv(here::here("data/gapminder5.csv"))
str(gapminder)

# Change factors to characters
gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)

# Finding the mean
mean(gapminder$lifeExp[gapminder$country == "Afghanistan"])

## For Loops ##

# recovering the total GDP for each country
obs <- 1:nrow(gapminder)

for (i in obs){
    gapminder[i, "gdp"] <- gapminder[i, "pop"] * gapminder[i, "gdpPercap"]
}

# creating a column containing the log of the GDP per capita and log of population
for (i in obs){
    gapminder[i, "log_gdpPercap"] <- log(gapminder[i, "gdpPercap"])
    gapminder[i, "log_pop"] <- log(gapminder[i, "pop"])
}

# mean life expectancy by year
years <- unique(gapminder$year)

for (i in years){
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i],
                    na.rm=TRUE)
    print(paste0(i, ": ", mean_le))
}

# mean life expectancy by continent
continents <- unique(gapminder$continent)

for (i in continents){
    mean_le <- mean(gapminder$lifeExp[gapminder$continent == i],
                                na.rm=TRUE)
    print(paste0(i, ": ", mean_le))
}

# mean life expectancy by continent for each year
for (i in continents){
    print(paste0("Continent ", i, ":"))
    for (j in years){
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & gapminder$year == j],
                        na.rm=TRUE)
        print(paste0(j, ": ", mean_le))
    }
}

# has the gap in life expectancy between countries on different continents narrowed over time?
# use standard deviation
for (i in continents){
    print(paste0("Continent ", i, ":"))
    for (j in years){
        std_dev <- sd(gapminder$lifeExp[gapminder$continent == i & gapminder$year == j],
                      na.rm=TRUE)
        print(paste0(j, ": ", std_dev))
    }
}

## Apply Function ##

# apply function will coerce to matrix
# apply(matrix, 1=row or 2=column, function)
vars <- gapminder[, c("lifeExp", "pop", "gdpPercap")]
apply(vars, 2, mean)

# lapply and sapply functions will return a list or simplified list (i.e., vector)
# sapply has some inconsistency in how it return results

lapply(gapminder, mean)

# find the mean LE by year using anonymous functions in apply
sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))

## While Loops ##

# use while loop to get standard deviation of LE for each year less than 1987
i <- 1952

while (i < 1987){
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le))
    i <- i + 5
}

# same but for years 1987 through 2002
i <- 1987
while (i <= 2002){
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i], na.rm=TRUE)
    print(paste0(i, ": ", sd_le))
    i <- i + 5
}

## Conditionals ##

# using if statements
set.seed(1)
random_year <- sample(years, 1)

if (random_year > 1977){
    print(random_year)
} else {
    print("Sorry, random year is less than 1977")
}

# reports the mean population for years >= 1987
for (i in unique(gapminder$year)){
    mean_pop <- mean(gapminder$pop[gapminder$year == i], na.rm=TRUE)
    
    if (i >= 1987){
        print(paste("The mean population in", i, "is", mean_pop))
    } else {
        print("The year is less than 1987")
    }
}

## Functions ##

# create a function that prints unique values for a column
get_values <-
    function(df, variable="continent"){
        vals <-unique(df[[variable]])
        print(paste0(variable, ": ", vals))
    }

# function that prints mean and std of a variable for given country
report_mean_sd <-
    function(df, variable, country){
        var <- df[[variable]][df$country == country]
        m_le <- mean(var)
        sd_le <- sd(var)
        cat("Country:", country, 
            "\nMean Life Expectancy:", m_le,
            "\nSD Life Expectancy:", sd_le)
    }

report_mean_sd(gapminder, "lifeExp", "Bulgaria")

# function that reports mean, median, minimum, and maximum LE for continent
report_stats <- 
    function(df, variable, continent){
        var <- df[[variable]][df$continent == continent]
        mean_var <- mean(var)
        median_var <- median(var)
        min_var <- min(var)
        max_var <- max(var)
        cat("Continent:", continent,
            "\nMean:", variable, mean_var,
            "\nMedian:", variable, median_var,
            "\nMin:", variable, min_var,
            "\nMax:", variable, max_var)
    }
report_stats(gapminder, "lifeExp", "Asia")
