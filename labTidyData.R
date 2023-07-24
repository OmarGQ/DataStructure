library(tidyverse)
library(ggplot2)

#Exercises
#1-Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
#  Carefully consider the following example:
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
#The order change and year became char when it was double

#2-Why does this code fail?
  table4a %>%
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
#1999 and 2000 must be between ''

#3-What would happen if you widen this table? Why? How could you add a new column to uniquely identify each value?
  people <- tribble(
      ~name,             ~names,  ~values,
    #-----------------|--------|------
    "Phillip Woods",   "age",       45,
    "Phillip Woods",   "height",   186,
    "Phillip Woods",   "age",       50,
    "Jessica Cordero", "age",       37,
    "Jessica Cordero", "height",   156
  )
#It would fail, because Phillip Woods has two ages.
people %>% pivot_wider(names_from = names, values_from = values)

#1-What do the extra and fill arguments do in separate()? Experiment with the various options for the following two toy datasets.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))

#2-Both unite() and separate() have a remove argument. What does it do? Why would you set it to FALSE?
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop", remove = FALSE)
  #Because it will show the extra values in a case like this

#3-Compare and contrast separate() and extract(). Why are there three variations of separation (by position,
# by separator, and with groups), but only one unite?

#Missing Values
#1-Compare and contrast the fill arguments to pivot_wider() and complete().

#2-What does the direction argument to fill() do?

whoCases <- who5 %>% group_by(country, year, sex) %>% summarise(count = sum(cases))
ggplot(data = diamonds, mapping = aes(x = clarity, y = price, fill="color")) + geom_violin()
