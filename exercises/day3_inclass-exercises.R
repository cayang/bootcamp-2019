# Load data
source("data/day3_objects.R")

# Creating plots - general syntax
# ggplot(data=<DATA FRAME>) + <GEOM_FUNCTION>(mapping=aes(<VARIABLES>))

# Creating scatterplot
ggplot(data=gapminder07) + 
    geom_point(mapping=aes(x=gdpPercap, y=lifeExp))

# Add labels
ggplot(data=gapminder07) + 
    geom_point(mapping=aes(x=gdpPercap, y=lifeExp)) +
    labs(title="Relationship between life expectancy and GDP per capita",
         x="GDP per capita", y="Life expectancy")

# Class exercise
# create variables for the natural log of gdpPercap and pop
gapminder07$log_gdpPercap <- log(gapminder07$gdpPercap)
gapminder07$log_pop <- log(gapminder07$pop)
gapminder07$log_lifeExp <- log(gapminder07$lifeExp)

# create scatterplot
ggplot(data=gapminder07) +
    geom_point(mapping=aes(x=log(pop), y=log(gdpPercap)) +
    labs(title="Relationship between population and GDP per capita",
         x="log(population)", y="log(GDP per capita)")
