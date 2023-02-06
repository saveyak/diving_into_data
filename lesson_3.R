#Diving into Data R Class Lesson 3: Clean, bind and visualize data from multiple years
#In this lesson, we will learn:
# - Why data cleaning is data analysis
# - What makes data "tidy"
# - How to pivot from "wide" to "long" data and vice-versa
# - Practice common data-cleaning tasks on real-world data
# - Write for loops to clean multiple dataframes and combine them all into one
# - Visualize data with ggplot2()

library(tidyverse)
library(readxl)
library(ggplot2)

# Introduction: Why data cleaning is data analysis ####

# Cleaning is an essential component of all data analysis projects.
# First, cleaning helps you understand the data better and watch out for pitfalls.
# Second, it is often impossible to perform a full analysis without cleaning the data first. Many functions just don't work on messy data.
#The data is clean when you can ask it questions without any headaches.
#Cleaning is not a separate process from data analysis -- it IS a form of data analysis. When you clean, you are making decisions that will potentially alter the data. You have to think about how that will impact your results.
# AI can write code for you but it can't make these kinds of decisions for you. That is what makes you a data analyst.

# Tidy up data ####

#To explore tidiness more, let's go back to the Louisiana enrollment data:

la22 = read_excel("./data/louisiana_enrollment/oct-2022-multi-stats-(total-by-site-and-school-system).xlsx", sheet=2, skip=5)

colnames(la22) = make.names(colnames(la22))

#Remember that before we summarized enrollment by race for Black and American Indian students. This is how we would do it for every race:

enrollment_by_race = la22 %>% group_by(School.System.Name) %>%
  summarize(amind = sum(AmInd),
            asian = sum(Asian),
            black = sum(Black),
            white = sum(White),
            hispanic = sum(Hispanic),
            hawaiian = sum(HawPI),
            multiple = sum(Multiple),
            minority = sum(Minority))

#Typing all of this out is very tedious and took 9 lines of code. If we make the data "tidy," it can make summarizing everything much more efficient.

#"Tidy" means every variable gets its own column. And every observation (a fact or figure about that variable) gets its own row.
#Right now we have no single column for the "race" variable. Instead we have a column for each race. So the data is not tidy.

#Another way to think about this is to ask: is the data "wide" (lots of columns) or "long" (lots of rows)? Long data is generally more tidy.

race_wide = la22 %>% select(School.System.Name, SiteName, AmInd:Minority)

race_long = la22 %>% select(School.System.Name, SiteName, AmInd:Minority) %>%
  pivot_longer(cols = AmInd:Minority,
               names_to="race",
               values_to="enrollment")

View(race_wide)
View(race_long)

#Now, if only we had a whole universe of packages solely devoted to tidying up data...
#Thankfully we do! Tidyverse allowed us to create the long data using pivot_longer. To break down what the code did:

#Cols = Select the range of columns that we want to collapse. We want to collapse every column from "AmInd" to "Minority."
#Names_to = Name the new column that will get filled with the names of the old columns. "AmInd", "Black", "Asian", etc. will now all be categories under "Race."
#Values_to = Name the new column that will get filled with the values of the old column. We named it "enrollment" because it shows the enrollment in each category.

enrollment_by_race = race_long %>% group_by(School.System.Name, race) %>%
  summarize(total = sum(enrollment))

View(enrollment_by_race)

#Now we can summarize by race in 2 lines of code instead of 9.
#Computers love long data. But humans sometimes find wider data easier to read. Use pivot_wider() to take the data above from long to wide:

enrollment_by_race %>% pivot_wider(names_from = "race", values_from="total") %>% View()

#Here's the outline for how to pivot longer or wider:
#pivot_longer(cols=colA:colZ, names_to="category", values_to="value")
#pivot_winder(names_from="category", values_from="value")

#Use pivot_longer() to collapse all the grade columns together, then filter for grade 3.





# Clean data on poverty and school finance ####

df = read_csv("./data/district_MO_construction_SY15_to_SY19.csv", skip=6)

#Make the headers easier to deal with
colnames(df) = make.names(colnames(df))

#That removed spaces we don't need all those periods. Use str_replace_all() to replace every group of one or more periods with an underscore.

colnames(df) = colnames(df) %>%
  str_replace_all(pattern="\\.+", replacement="_")

#What does "\\.+" means? It is a regular expression, which is a code that allows us to find patterns in strings.
# \\. ---> find a period (we need to type \\ because normally a period means "find any character" in regex, but we literally want a period here)
# +   ---> the period can be repeated any number of times

#Find the cheatsheet of regular expressions here: https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf

#Better, but these names are still too long. Use str_replace() to replace some long chunks of text with blanks.

colnames(df) = colnames(df) %>%
  str_replace("_All_Grades_Excludes_AE_District","") %>%
  str_replace("_Supp_Serv_Exp_V40_District_Finance","") %>%
  str_replace("_Capital_Outlay_F12_District_Finance","")

#A few column names are still too long. We'll rename them individually with rename().

df = rename(df, State=State_Name_District_Latest_available_year,
            NCES=Agency_ID_NCES_Assigned_District_Latest_available_year)

#Rename Agency_Name to just Agency






df = rename(df, Agency=Agency_Name)

#At the bottom of the spreadsheet there are some notes, which we can cut out after reading them.

#We could do this
df %>% slice(1:20467)


#But what if we get updated data from the government and it doesn't have exactly 20467 rows anymore?

#Instead, use drop_na() to drop all rows that are NA within the column "NCES."
df = df %>% drop_na("NCES")



#There are lots of different types of district in this data. Use unique() to list each unique type.

unique(df$Agency_Type_District_2018_19)

df %>% count(Agency_Type_District_2018_19)


#The data also tells us if all the schools in a district are charter, noncharter or both.
unique(df$Agency_Charter_Status_District_2017_18)


# Do we want to filter out charter schools? Do we only want regular local school districts? Do we want districts run by state or federal agencies? What do you think?



# Check on some school districts you're familiar with to see if there are any red flags. Example: New York City has multiple geographic districts and a supervisory union district, found under the name "NYC Chancellor's Office." The geographic districts have data on the number of students but no data on spending, and vice-versa for the chancellor's office.




#Let's deal with null values like †.

#One option: Replace all cells that are just "†" to NA
df[df == "†"] <- NA

#Could do the same for the double-cross and hyphen. Another option: mutate columns 8 to the last column to be numeric. All non-numbers will automatically become NAs.

df = df %>% mutate_at(c(8:ncol(df)), as.numeric)



#Do we want to include extremely small districts? How do we define small -- 100, 500, 50 students?
#We probably want districts that have at least 100 students for all five years.

df = df %>% filter(if_all(starts_with("Total_Students"), ~ .x >= 100))

#Translation: filter if all the columns that start with "Total_Students" are greater than or equal to 100
#You can also use if_any() instead of if_all()

df %>% count(Agency_Type_District_2018_19)
#This method allowed us to get read of most really weird districts. We mostly have regular districts and charters.

#Let's check if any districts are duplicated; we would want to filter them out if they were.
subset(df,duplicated(NCES))
#They are not.

#The data is now sufficiently clean. We can join in poverty data from the Census and see whether poorer schools spend more or less on maintenance and construction.

poverty = read_excel('./data/ussd20.xls', skip=2)

#This data comes from the Small Area Income and Poverty Estimates dataset from the Census. What other ways could we measure income?
#https://www.census.gov/programs-surveys/saipe.html

#This dataset also needs to be cleaned up a bit.
#Create new column names
colnames(poverty) = c("state","state_fips","district_ID", "name", "total_pop", "pop_5to17", "pop_5to17_in_poverty")

#To join this dataset to the construction dataset, we need to join on the NCES ID. The NCES ID is the same as the state fips plus the district ID.

poverty = poverty %>% mutate(NCES=paste0(state_fips, district_ID))

#Could also write it like this:
#poverty$NCES = paste0(poverty$state_fips, poverty$district_ID)
#Or you could use str_c() which works similarly to paste0

#Create a column called pct_children_in_poverty
poverty$pct_children_in_poverty = poverty$pop_5to17_in_poverty / poverty$pop_5to17 * 100

#What's an alternate way to do this with mutate()?



#Join df and poverty

df = left_join(df, poverty, by="NCES")


# Some districts don't have poverty data -- mostly charters.

df %>% filter(is.na(pct_children_in_poverty)) %>% count(Agency_Type_District_2018_19)

#Filter to include only the districts where pct_children_in_poverty is NOT N/A
df = df %>% filter(!is.na(pct_children_in_poverty))


# What is the average amount spent in maintenance and construction spending per student per year, by poverty level, over this five-year period?

df$five_year_totals_ops = df %>% select(starts_with("Total_Ops")) %>% rowSums(.)
df$five_year_totals_construction = df %>% select(starts_with("Construction")) %>% rowSums(.)
df$five_year_totals_students = df %>% select(starts_with("Total_Students")) %>% rowSums(.)

#How do we want to characterize poverty? By quartiles, quintiles? For the nation as a whole, or relative to the state each district is in?




#Let's go with within-state poverty quintile for each state (supported by the literature: https://www.barbarabiasi.com/uploads/1/0/1/2/101280322/pp_schoolconstruction.pdf)
#Remember, quintile 1 is richest and quintile 5 is poorest
df = df %>% group_by(state) %>%
  mutate(state_poverty_quintile=ntile(pct_children_in_poverty, 5)) %>%
  ungroup()

#Let's compare how the quintile ranges differ in richer and poorer states

df %>% filter(State == "Mississippi" | State == "Connecticut") %>%
  group_by(State, state_poverty_quintile) %>%
  summarize(lower_bound = min(pct_children_in_poverty),
            upper_bound=max(pct_children_in_poverty))


#What do school districts spend on maintenance and operation every year, by poverty level?
df %>% group_by(state_poverty_quintile) %>%
  summarize(mo=sum(five_year_totals_ops, na.rm=T),
            students=sum(five_year_totals_students, na.rm=T)) %>%
  mutate(mo_per_student = mo/students)

#Perform the same operation as above, but this time look at construction spending (five_year_totals_construction)





# Write for loops to automate data-cleaning ####

#We were able to clean up data from 2022. What if we wanted to look at enrollment trends going back to 2017? We could write out the same code over and over again to clean each file, then bind them all together. Or we could write code, known as a for loop, that automatically does this for us.

#Example: R comes built-in with a list of fruit, for whatever reason.

fruit

#Say we want to print out the name of each fruit, one at a time...
print("apple")
print("apricot")
print("avocado")
#Obviously printing out all of them would take forever. Instead we can use a for loop.

for (f in fruit) {
  print(f)
}

#The layout for a for loop is like this:
#for (x in a_range_of_numbers_or_a_list) {
#  someFunction(x)
#}

for (f in fruit) {
  f = toupper(f)
  print(paste0("I like to eat ", f,"S!"))
}

#You can also specify a range of numbers
for (i in 1:5) {
  print(i)
}

for (f in fruit[1:5]) {
  f = toupper(f)
  print(paste0("I like to eat ", f,"S!"))
}


for (i in 1:length(fruit)) {
  print(i)
}

#Same loop but make it print "I ate (i*10) fruits"





#Let's use a for loop for a real-life dataset.

files = list.files("./data/louisiana_enrollment/")


#Go through each file in the list of files and print its name.
for (f in files) {
  print(f)
}


#Go through each file in the list of files and print its name, then create a dataframe.
for (f in files) {
  filepath = paste0("./data/louisiana_enrollment/", f)
  print(filepath)
  df = read_excel(filepath, skip=5)
}

#Problem: we only see the last dataframe that was created. Every time we made a new df, we overwrote the old one.

#We need to create an empty dataframe called la_grades. Every time we create a new df, we bind it to the la_grades so that the data gets saved.

la_grades = data.frame()

for (f in files) {
  filepath = paste0("./data/louisiana_enrollment/", f)
  print(filepath)
  df = read_excel(filepath, skip=5)

  la_grades = rbind(la_grades, df)
}

#Unfortunately, this does not work because the Excel spreadsheets didn't all have the exact same headers each year. We have to clean the data so that it's uniform every year, and then we can bind it together.

la_grades = data.frame()

for (f in files) {
  filepath = paste0("./data/louisiana_enrollment/", f)
  print(filepath)
  df = read_excel(filepath, skip=5) %>%
    filter(`School System Name`=="Statewide total") %>%
    select(PreK:Grade12) %>%
    pivot_longer(PreK:Grade12, names_to="grade", values_to="enrollment") %>%
    filter(grade != "GradeT9")
  df$census_date = str_extract(filepath,"oct-20..") #extracts "oct-20" and then the two following characters

  la_grades = rbind(la_grades, df)
}

#Why do you think I added the line df$census_date = str_extract(filepath,"oct-20..")?

View(la_grades)

#Do you notice something is missing?
#In 2017, the DOE wrote "Totals" instead of "Statewide total." A tiny difference like this leads to everything getting filtered out of the 2017 dataframe.

#How can we update the code so that it filters for "Statewide total" OR "Totals"?







la_grades = data.frame()

for (f in files) {
  filepath = paste0("./data/louisiana_enrollment/", f)
  print(filepath)
  df = read_excel(filepath, skip=5) %>%
    filter(`School System Name`=="Statewide total" | `School System Name` == "Totals") %>%
    select(PreK:Grade12) %>%
    pivot_longer(PreK:Grade12, names_to="grade", values_to="enrollment") %>%
    filter(grade != "GradeT9")
  df$census_date = str_extract(filepath,"oct-20..") #extracts "oct-20" and then the two following characters

  la_grades = rbind(la_grades, df)
}

View(la_grades)

#We want to calculate year-over-year change
#Let's use a function called lag() to create a column that takes its value from the row right above it. This will allow us to calculate the value from the previous year, and then compare it to the current year.
#But, we need to skip the first year, 2017, because of course there's no previous value in that case.

la_grades = la_grades %>%
  arrange(grade) %>%
  mutate(previous_year_value = case_when(
    census_date == "oct-2017" ~ NA_real_,
    TRUE  ~ lag(enrollment, order_by=grade),
  ))

#Note: We have to specify NA_real_ instead of just NA when we use case_when() and have a mix of NA's and real numbers (a.k.a doubles or non-integers.)
#More info on why it has to be NA_real_: https://www.njtierney.com/post/2020/09/17/missing-flavour/

#Mutate to calculate year-over-year changes
la_grades = la_grades %>%
  mutate(yoy_change = (enrollment-previous_year_value)/previous_year_value*100)

View(la_grades)

#Round year-over-year change to two decimal places
la_grades = la_grades %>% mutate(yoy_change = round(yoy_change, 2))

#What trends do you notice in the data?
#What happened to the preschool and kindergarten population when the pandemic hit?





#Plot data####

#Exploratory visualization is an essential part of data analysis. The purpose here isn't necessarily to make nice graphics for publication (although you can do that); the purpose is to help understand your data better.

#Ggplot is the standard way to make graphics. For every plot you need to specify three things:

#1) The data that you're using
#2) The aesthetics (aes) -- how you want to the data to look. This is where you can control goes on the x-axis and y-axis, the height of bars in a bar chart, color of lines in a line chart, shape and size of points on a scatterplot, etc.
#3) The geometry (geom) -- what kind of graph you're making. E.g.: Histogram, bar chart, line graph, box plot, etc.

#Let's start with a simple bar graph for one year.
la22 = la_grades %>% filter(census_date=="oct-2022")

ggplot(data=la22, aes(x=grade, y=enrollment)) +
  geom_bar(position="dodge", stat="identity")

#Problem: the grades are not in order. We want it to be arranged from PreK to 12th.

glimpse(la_grades)

#We want to change grades from a character to what's known as a "factor." A factor is like a character that has to go in a particular order. We specify that order with the "levels" parameter.

grade_order = c("PreK", "Kindergarten", "Grade1", "Grade2", "Grade3", "Grade4", "Grade5", "Grade6", "Grade7", "Grade8", "Grade9", "Grade10", "Grade11", "Grade12")

la22$grade = factor(la22$grade, levels=grade_order)

glimpse(la22)

#Now we can make a bar char in the right order
ggplot(la22, aes(x=grade, y=enrollment)) +
  geom_bar(stat="identity")

#Let's refactor the entire la_grades dataframe as well.
la_grades$grade = factor(la_grades$grade, levels=grade_order)

#Let's do a bar plot that shows each year

#Bar plot, enrollment in each grade over time. We specify fill=census_date so there will be a bar of a different color for each date.
ggplot(la_grades,aes(x=grade, y=enrollment, fill=census_date)) +
  geom_bar(stat="identity")

#It's stacking each year on top of each other instead of stacking them next to each other. Specify position="dodge" to avoid this problem.
ggplot(la_grades,aes(x=grade, y=enrollment, fill=census_date)) +
  geom_bar(position="dodge", stat="identity")

#Let's change the colors
ggplot(la_grades,aes(x=grade, y=enrollment, fill=census_date)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Purples")

#This graph is OK but it's all too much visual information stuffed into one grid. Using small multiples is a great way to break down complex visualizations into more understandable chunks.
#Use facet_wrap to break graph into multiple small graphs.

ggplot(la_grades,aes(fill=grade, y=enrollment, x=grade)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ census_date)

#To flip the X and Y axis so we have horizontal bars, use coord_flip()
ggplot(la_grades,aes(fill=grade, y=enrollment, x=grade)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ census_date) +
  coord_flip()

#Note the distinct dip around late elementary/middle school


#Let's say we wanted line charts instead of bar graphs. Sometimes having just a simple line cuts out distracting visual information.
#Make a line chart for just 2022:
ggplot(la22, aes(x=grade, y=enrollment, group=1)) +
  geom_line()

#Same line plot, but with a dot for each grade
ggplot(la22, aes(x=grade, y=enrollment, group=1)) +
  geom_line() +
  geom_point()

#For line graphs you need to specify a grouping of datapoints that will be connected. In this case all the datapoints are connected because we are only looking at one year of data, so group=1.
#If instead we wanted to have multiple lines for a particular variable, we would specify the group as that variable.

ggplot(la_grades, aes(x=grade, y=enrollment, group=census_date)) +
  geom_line()


#Let's have all the lines be different colors
ggplot(la_grades, aes(x=grade, y=enrollment, group=census_date, color=census_date)) +
  geom_line()

#This line chart is not that useful due to having too many lines. Small multiples can again help us.

ggplot(la_grades,aes(group=census_date, y=enrollment, x=grade)) +
  geom_line() +
  facet_wrap(~ census_date)
#Look at how enrollment craters in late elementary/middle school.

#Look at the trend for each grade
ggplot(la_grades,aes(group=grade, y=enrollment, x=census_date)) +
  geom_line() +
  facet_wrap(~ grade)
#Shows how every grade is below pre-pandemic level except 9, 10

#Graph percent change, with small multiples of years
la_grades %>% filter(census_date !="oct-2017") %>%
  ggplot(aes(fill=grade, y=yoy_change, x=grade)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ census_date)

#Graph percent change, with small multiples of grades
la_grades %>% filter(census_date !="oct-2017") %>%
  ggplot(aes(fill=census_date, y=yoy_change, x=census_date)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ grade)

#Click export and then "save as image" to save your chart. If you save it as an SVG, you can then open it in Illustrator and edit it there. You can also use to the ggsave() function to save.

#This is only barely scratching the surface of data visualization in R. An entire course could be devoted to just that topic.
#To learn more, one of the best guides to Ggplot2 is STHDA: http://www.sthda.com/english/wiki/ggplot2-essentials
#I also strongly suggest you read the guides to data visualization and spatial analysis (i.e. mapping) written by Andrew Ba Tran of the Washington Post: https://learn.r-journalism.com/en/visualizing/

