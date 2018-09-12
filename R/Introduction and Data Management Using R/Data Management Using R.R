#########################################################################################
#       Name:   Data Management Using R.R                                               #
#       Date:   September 12, 2018                                                      #
#       Author: Desmond D. Wallace                                                      #
#       Purpose:        Basic data managmement for first time users via the tidyverse.  #
#########################################################################################


# Install and load needed packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("gapminder",
              "rio",
              "tidyverse")

## gapminder - Features excerpt of the Gapminder data on life expectancy, GDP per capita, and population by country.
## rio - "A Swiss-Army Knife for Data I/O"
## tidyverse - The tidyverse is an opinionated collection of R packages designed for data science. All packages share an underlying design philosophy, grammar, and data structures.

ipak(packages)

# Load gapminder data

exData <- gapminder

# Export/Import Data

export(exData,
       file = "./gapminder.csv") # File type determined by file extension

convert(in_file = "./gapminder.csv",
        out_file = "./gapminder.dta")

exData2 <- import(file = "./gapminder.dta")

## For all supported file types, see https://cran.r-project.org/web/packages/rio/vignettes/rio.html

# Data Manipulation

## arrange - Arrange rows by variables

### Which countries have the largest populations?

arrange(gapminder,
        desc(pop))

## filter - Return rows with matching conditions

### Which countries had the largest population in 2007?

#### Traditional Approach (Read function inside out)

arrange(filter(exData,
               year == 2007),
        desc(pop))

#### Pipe (%>%) Approach (Read function left to right)

exData %>%
        filter(year == 2007) %>%
        arrange(desc(pop))

## select - Select/rename variables by name

### Which countries had the largest life expectancy in 2007?

exData %>%
        filter(year == 2007) %>%
        arrange(desc(lifeExp)) %>%
        select(country, lifeExp)

## mutate/transmute - Add new variables

### What is the Gross Domestic Product (GDP) of each country?

exData %>%
        mutate(gdp = pop * gdpPercap)

exData %>%
        transmute(gdp = pop * gdpPercap)

### NOTE: mutate adds variables only; transmute adds variables and removes other variables

## summarise - Reduces multiple values down to a single value

### What was the average GDP?

exData %>%
        mutate(gdp = pop * gdpPercap) %>%
        summarise(mean_gdp = mean(gdp))

## group_by - Group by one or more variables

### What was the average GDP for each year?

exData %>%
        mutate(gdp = pop * gdpPercap) %>%
        group_by(year) %>%
        summarise(mean_gdp = mean(gdp))

## bind_rows/bind_cols - Efficiently bind multiple data frames by row and column

### bind_rows - Appends objects

exData0207 <- bind_rows(exData %>%
                                filter(year == 2002),
                        exData %>%
                                filter(year == 2007))

### bind_cols - Merges objects

exDataAB <- bind_cols(exData %>%
                              filter(year == 2002) %>%
                              select(country,
                                     continent),
                      exData %>%
                              filter(year == 2002) %>%
                              select(country,
                                     year))
