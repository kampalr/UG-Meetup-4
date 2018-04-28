# Loading our tibble libraray
library(tibble)

#Importing our first data set for uace student 2011 to 2015
uaceexams <- as_tibble(read.csv("uaceexams.csv", stringsAsFactors = F,check.names = F))

# checking the content of our data set
uaceexams

# Loading tidyr library
library(tidyr)
#loading magrittr for %>%
library(magrittr)
# gathering our data set to make all column names variables
uaceexams %>% gather(`2011`, `2012`, `2013`, `2014`,`2015`,key = "year", value = "Students")
# assign an object to our data
newtestuace <- uaceexams %>% gather(`2011`, `2012`, `2013`, `2014`,`2015`,key = "year", value = "Students")
# reading our second data set
women_abused <- as_tibble(read.csv("sexualviolence.csv",stringsAsFactors = F, check.names = F))
# cheching our data
women_abused
# spreading our dataset 
women_a <- spread(women_abused, key = Type, value = Count)

#Mapping
library(purrr)
testmap <- tibble(a=rnorm(15),b = rnorm(15), c= rnorm(15),d = rnorm(15))
map_dbl(testmap, mean)
testmap %>% map_dbl(mean)

#dplr package
women_a <- spread(women_abused, key = Type, value = Count)
filter(women_a, Percentage > 23.5)
filter(women_a, women_a$Percentage > 23.5)
library(dplyr)
filter(women_a, Percentage > 23.5)
arrange(women_a, desc(Percentage))
head(arrange(women_a, desc(Percentage)))
tail(arrange(woman_a, desc(Percentage)))
select(women_a, starts_with("P"))
mutate(women_a, Mens_percentage = (1/3)*Percentage)
women_a%>% summarise(Total = sum(Women))
men_a <- as_tibble(read.csv("men_abuse.csv",stringsAsFactors = F, check.names = F))
bind_cols(women_a,men_a)
bind_rows(women_a,men_a)
as.data.frame(bind_rows(women_a,men_a))
union(women_a,men_a)
left_join(women_a,men_a, by= c("Period","Background","Particulars"))
sexviolence <- left_join(women_a,men_a,by=c("Period","Background","Particulars"))
inner_join(women_a,men_a, by= c("Period","Background","Particulars"))

#forcats
library(forcats)
monthvector <- c("Feb", "Apr", "Jan", "Mar")
sort(monthvector)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
factor(monthvector, levels = months)
sort(factor(monthvector, levels = months))
ggplot(women_a, aes(Percentage, fct_reorder(Particulars, Percentage))) + geom_point()

ggplot(men_a, aes(Percent_Men, fct_reorder(Particulars, Percent_Men))) + geom_point()
women_a%>%mutate(Particulars= fct_collapse(Particulars,Western = c("Bakiga", "Bakonzo", "Banyankore", "Banyoro","Batoro"), Eastern = c("Basoga", "Iteso","Bagisu"),Northern = c("Acholi", "Lango"), WestNile = c("Alur", "Lugbara"), Other = "Others"))
women_a%>%mutate(Particulars= fct_collapse(Particulars,Western = c("Bakiga", "Bakonzo", "Banyankore", "Banyoro","Batoro"), Eastern = c("Basoga", "Iteso","Bagisu"),Northern = c("Acholi", "Lango"), WestNile = c("Alur", "Lugbara"), Other = "Others")) %>%count(Particulars)

# part 2 Visualization
ggplot(data = newtestuace) +     geom_point(mapping = aes(x = Gender, y = Students, color= year), na.rm = T)
#facets
ggplot(data = newtestuace) +     geom_point(mapping = aes(x = Gender, y = Students)) +     facet_wrap(~ year, nrow = 2)
#position adjustment
ggplot(data = newtestuace) +    geom_bar(mapping = aes(x = Students, fill = year))
newtestuace1 <- filter(newtestuace, Students < 100)
ggplot(data = newtestuace1) +     geom_bar(mapping = aes(x = Students, fill = Gender), position = "dodge")
# interpreting boxplots
ggplot(data = newtestuace, mapping = aes(x = year, y = Students)) + geom_boxplot()

