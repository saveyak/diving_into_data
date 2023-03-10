#Diving into Data R Class Lesson 2: Manipulate data
#In this lesson, we will learn:
# - The Stupendous Six: filter(), mutate(), select(), summarize(), group_by(), arrange()
# - How to pivot data from wide to long so it's more "tidy"
# - Compute basic stats like mean, median, quartiles
# - Compute sums across rows and columns
# - How to join data

library(tidyverse)
library(readxl)

# Filter() and arrange() ####

#Tab 2 of this file shows the enrollment information for each school in Louisiana as of October 2022.
la22 = read_excel("./data/louisiana_enrollment/oct-2022-multi-stats-(total-by-site-and-school-system).xlsx", sheet=2, skip=5)

#Filter the data so that it only includes schools that are run by the FirstLine Schools nonprofit

la22 %>%
  filter(Nonprofit == "FirstLine Schools, Inc.") %>%
  View()

#Notice that we are using TWO equal signs. Don't confuse with the assignment operator which is ONE equal sign
#Firstline Schools, Inc. has to be in quotation marks because it is a string

#Let's say we want every school EXCEPT FirstLine schools. != means "does not equal"
#Note that this will also get rid of all schools that have NA under "Nonprofit"
la22 %>%
  filter(Nonprofit != "FirstLine Schools, Inc.") %>% View()

#Filter for the school that has the Site Code 026075
la22 %>% filter(SiteCd == "026075") %>% View()

#Filter for ONLY the statewide total
la22 %>% filter(SiteName == "Total") %>% View()


#Filter OUT the statewide total.
la22 %>% filter(SiteName != "Total") %>% View()



#Filter for schools with more than 100 kindergarteners
la22 %>% filter(Kindergarten > 200)

# > more than
# < less than
# >= more than or equal to
# <= less than or equal to

#Filter for schools with fewer than 100 kindergarteners


la22 %>% filter(Kindergarten < 100) %>% View()


#Filter for only schools in New Orleans (which has Parish Code 36)
la22 %>% filter(`Parish Code` == 36)
#Parish Code has to be in tick marks (NOT quote marks) because we are calling a column name with a space in it.

#Filter for schools that are more than 30% students with limited-English proficiency (LEP)
la22 %>% filter(`%LEP` > 0.3)
#R doesn't like the % sign to be in column names either

#These tick marks are annoying to deal with. Use the make.names function to remove spaces and unwanted characters like %

colnames(la22)

colnames(la22) = make.names(colnames(la22))

colnames(la22)

#Filter for only schools that are NOT in New Orleans
la22 %>% filter(Parish.Code != 36)


#Filter for schools that are NOT in Jefferson Parish (parish code 26)
la22 %>% filter(Parish.Code != 26)


#Filter and then count how many schools are run by each nonprofit
la22 %>% filter(Parish.Code == 36) %>% count(Nonprofit) %>% View()

#Use arrange() to sort the data
la22 %>% filter(Parish.Code == 36) %>% count(Nonprofit) %>%
  arrange(n) %>% View()

#Arrange be default puts the smallest values on top. Put a minus sign before the column name to reverse this.
la22 %>% filter(Parish.Code == 36) %>% count(Nonprofit) %>%
  arrange(-n) %>%
  slice(1:10)


#You can combine multiple filters with and/or operators
#Filter for schools that are in New Orleans AND a Type 1 charter with the "&" operator
la22 %>% filter(Parish.Code == 36 & Charter.Type == "Type 1")

#Filter for schools that are in New Orleans OR a Type 1 charter with the "|" operator
la22 %>% filter(Parish.Code == 36 | Charter.Type == "Type 1")

#Use %in% to filter based on a list
#Filter for schools in the New Orleans suburbs
suburbs = c("Jefferson Parish", "St. Bernard Parish", "St. Tammany Parish", "Plaquemines Parish")

la22 %>% filter(School.System.Name %in% suburbs) %>% View()

#Filter for suburbs then arrange by total students -- smallest schools on top
la22 %>% filter(School.System.Name %in% suburbs) %>%
  arrange(Total.Students) %>% View()

#Same thing but with the biggest schools on top
la22 %>% filter(School.System.Name %in% suburbs) %>%
  arrange(-Total.Students) %>% View()




#Filter for schools with more than 80 and fewer than 100 kindergarteners
la22 %>% filter(Kindergarten > 80 & Kindergarten < 100) %>% View()


#Filter for all rows where the school has "Elementary School" in the name
#We use str_detect which detects a pattern within a string. It returns TRUE if the pattern is detected or FALSE if it is not.
str_detect(string=la22$SiteName, pattern="Elementary School")

str_detect(la22$SiteName, "Elementary School")


#When combined with filter, it keeps all rows that returned a TRUE value
la22 %>% filter(str_detect(SiteName, "Elementary School")) %>% View()


#Arrange schools by number of kindergartners, then find the top 5 schools with the most Kindergarteners

la22 %>% arrange(-Kindergarten) %>%
  slice(5:1) %>% View()



#Filter for schools with greater than average share of students with limited English proficiency (LEP)
#mean() is used to find the average (note: our data has no NA's; we'll see later how to modify the code when there are NA's)

mean(la22$X.LEP)

la22 %>% filter(X.LEP > mean(X.LEP)) %>% View()


#Same as above, but arrange it so schools with the highest share of LEP students are on top
la22 %>% filter(X.LEP > mean(X.LEP)) %>%
  arrange(desc(X.LEP)) %>%
  View()



#Filter for schools that have TWICE the average share of LEP students

la22 %>% filter(X.LEP > 2 * mean(X.LEP)) %>% View()


#Filter for schools that are above-average in terms of ED (economically disadvantaged) kids
# ED.

avg_eco_dis = mean(la22$ED.)

la22 %>% filter(ED. > avg_eco_dis) %>% View()


#Find all the schools that are 100% economically disadvantaged
la22 %>% filter(ED. == 1) %>% View()



# Select() + selection helpers ####

#Use select() to select columns from a dataframe. You can select by column name or number.

#Select site name and total number of students
la22 %>% select(SiteName, Total.Students)
la22 %>% select(5:6)

#Select site name and all columns from "AmInd" to "Minority"
la22 %>% select(SiteName, AmInd:Minority)
la22 %>% select(5, 9:16)

#Select the last column
la22 %>% select(Parish.Code)
la22 %>% select(ncol(la22))
la22 %>% select(last_col())


#Select everything EXCEPT the site name
la22 %>% select(-SiteName)

#Select site name, site code
la22 %>% select(SiteName, SiteCd)


#Select kinder to grade 12

la22 %>% select(Kindergarten:Grade12)

#Select site name, site code, and kinder to grade 12

la22 %>% select(SiteCd, SiteName, Kindergarten:Grade12)


#Select all columns from AmInd to Multiple, except HawPI
la22 %>% select(AmInd:Multiple, -HawPI)


#Columns can be selected in any order
la22 %>% select(Parish.Code, SiteName, School.System.Name)



#This can be useful for rearranging the data
#Put the school name first, then everything else
la22 %>% select(SiteName, everything())



#Select the school name first, then race-related columns, then everything else
la22 %>% select(SiteName, AmInd:Multiple, everything()) %>% View()


#Selection helpers allow you to select columns that start with, end with, or contain certain string
la22 %>% select(starts_with("Grade"))



la22 %>% select(starts_with("School"))



la22 %>% select(contains("X."))


#Find all the columns that contain "male" (or "Male")
#Note: by defaults selection helpers are case-insensitive unless you add ignore.case=FALSE
la22 %>% select(contains("male"))



#Select school name and grades K-12, but not GradeT9
la22 %>% select(SiteName, Kindergarten:Grade12, -GradeT9)


#You can rename columns at the same time you select them
la22 %>% select(SiteName, percent_limited_english=X.LEP)


#The other way to rename columns is with rename()
#la22 = rename(la22, percent_limited_english=X.LEP)

rename(la22, percent_limited_english=X.LEP)


#Select school name, school code, and percent economically-disadvantaged. Come up with a new name for the last column.
la22 %>% select(SiteName, SiteCd, low_income=ED.)






# Mutate() ####
#Mutate allows you to create new columns based on existing columns.

df

#Take our student dataframe from before and add an age_in_dog_years column
df %>% mutate(age_in_dog_years = age*7)

#Add a column for first name in upper case letters
df %>% mutate(first_name_but_yelling = toupper(first))

#Mutate can also modify existing columns instead of adding new ones.
df %>% mutate(first = toupper(first))

#Can modify multiple columns at once
df %>% mutate(first = toupper(first),
              last = toupper(last))

#str_to_title() to make it title case


#Add or modify a column where the organization is all lower case
df %>% mutate(lower_org = tolower(org))


#For la22 dataframe, add an Early Childhood Total column for PreK, K and 1st grade

la22 %>% mutate(early_childhood_total = PreK + Kindergarten + Grade1) %>%
  select(SiteName, PreK:Grade1, early_childhood_total)



#Add a column for the K-8 total
#rowSums() is a function that sums values in a row and across() is a function that allows you to apply a function to several columns.

#Add a column for total enrollment in grades K-8
la22 %>% mutate(total_k8 = rowSums(across(Kindergarten:Grade12))) %>%
  select(SiteName, total_k8)

#Add a column for total enrollment in grades 9-12
la22 %>% mutate(total_9_12 = rowSums(across(Grade9:Grade12))) %>%
  select(SiteName, total_9_12)


#Add a column for percent minority
la22 %>% mutate(pct_minority = Minority/Total.Students * 100) %>%
  select(SiteName, pct_minority)






#Ifelse() is similar to the =IF formula in Excel
la22 %>% mutate(predominant_gender = ifelse(test = X.Female >= 0.5,
                                            yes = "Female",
                                            no = "Male")) %>%
  select(SiteName, X.Female, predominant_gender)

#Normally this is written in a shorter format, like like =IF in Excel.
la22 %>% mutate(predominant_gender = ifelse(X.Female >= 0.5, "Female", "Male"))

#Use case_when to easily add columns based on multiple possible if/else conditions. NO MORE NESTED IF STATEMENTS! EVER!!

la22 %>% mutate(predominant_gender = case_when(
  X.Female > 0.5 ~ "Female",
  X.Female == 0.5 ~ "Equally Male and Female",
  TRUE ~ "Male"
  )) %>%  select(SiteName, X.Female, X.Male, predominant_gender)

##TRUE here means "in all other cases."
#We could also write: X.Male > 0.5 ~ "Male"

#Classify schools as large, small or medium
la22 %>% mutate(school_size = case_when(
  Total.Students < 500 ~ "Small",
  Total.Students >= 500 & Total.Students < 1000 ~ "Medium",
  TRUE ~ "Large"
)) %>%
  select(SiteName, Total.Students, school_size)

#Classify schools as elementary, middle, high schools or other

la22 %>% mutate(school_type = case_when(
  str_detect(SiteName, "High School") ~ "High School",
  str_detect(SiteName, "Elementary School") ~ "Elementary School",
  str_detect(SiteName, "Middle School") ~ "Middle School",
  TRUE ~ "Not sure"
)) %>% select(SiteName, school_type)

#Use case_when to add a column that marks whether a school is located in "New Orleans Proper", "New Orleans Suburbs," or "Elsewhere in Louisiana"

suburbs = c("Jefferson Parish", "St. Bernard Parish", "St. Tammany Parish", "Plaquemines Parish")

la22 %>% mutate(location = case_when(
  Parish.Code == 36 ~ "New Orleans Proper",
  School.System.Name %in% suburbs ~ "New Orleans Suburbs",
  TRUE ~ "Elsewhere in Louisiana"
)) %>% select(School.System.Name, SiteName, location)



#Use ntile() to classify schools into quartiles based on school size. Quartile 1 will have the 25% smallest schools in Louisiana and Quartile 4 will have the 25% largest schools in Louisiana.

la22 %>% select(SiteName, Parish.Code, Total.Students) %>%
  mutate(quartile = ntile(Total.Students, n=4)) %>%
  arrange(Total.Students) %>%
  View()

#The ntile() function can also group variables into quintiles (n=5), deciles (n=10), percentiles (n=100), etc.

#Assign schools to quartiles based on the percent of students who are disadvantaged, relative to the state as a whole
la22 %>% select(SiteName, School.System.Name, Parish.Code, ED.) %>%
  mutate(quartile = ntile(ED., n=4)) %>%
  arrange(ED.) %>%
  View()

#Quartile 1 represents the least economically disadvantaged schools and quartile 4 represents the most economically disadvantaged schools.


# Group_by() and Summarize() ####
#You use group_by before another function, to specify that you would like that function to be applied within specific groups.

#Get a random sample of four schools
la22 %>% slice_sample(n=4) %>% select(Parish.Code, SiteName)

#Get a random sample of one school from each parish
la22 %>% group_by(Parish.Code) %>% slice_sample(n=1) %>% select(Parish.Code, SiteName) %>% ungroup()



#SKIP TO LINE 419 ##

#Previously, we assigned districts to quartiles based on how they ranked within the state as a whole.
#We can use group_by to instead assign districts to quartiles based on how they rank within their parish.
#Now assign schools to quartiles relative to the PARISH they're in
la22 %>% select(SiteName, School.System.Name, Parish.Code, ED.) %>%
  group_by(Parish.Code) %>%
  mutate(quartile = ntile(ED., n=4)) %>%
  arrange(ED.) %>%
  View()


#Schools in quartile 1 are now the least economically disadvantaged IN THEIR PARISH. Schools in quartile 4 will be the most economically disadvantaged in their parish.

#Important note on group_by: if we were actually changing the dataframe and not just messing around, we would want to put %>% ungroup() as the last line. This ensures we don't accidentally mess up our analysis by keeping the data grouped into parishes when we want to look at the entire state.

la22_quartiles = la22 %>% select(SiteName, School.System.Name, Parish.Code, pct_disadvantaged = ED.) %>%
  group_by(Parish.Code) %>%
  mutate(quartile_within_parish = ntile(pct_disadvantaged, n=4)) %>%
  arrange(pct_disadvantaged) %>%
  ungroup()



#Summarize(): This is like pivot tables in Excel. Use it to create a new table that gives you summary details about the data.
#median() gives you the median, mean() gives you the average, n() gives you the number of observations

la22 %>% summarize(median_pct_disadvantaged = median(ED.),
                   average_pct_disadvantaged = mean(ED.),
                   number_of_schools = n())

#Summarize() is almost always paired with group_by() first.
#Same thing as above, but now we are finding these stats for every school system

la22 %>% group_by(School.System.Name) %>%
  summarize(median_pct_disadvantaged = median(ED.),
            average_pct_disadvantaged = mean(ED.),
            number_of_schools = n())

#Other useful summary functions: n(), n_distinct(), first(), last(), nth()
#abs(), max(), min(), sum(), range(), quantile(), sd(), median(), mean()

#Find the enrollment in the smallest and largest schools in each school system
la22 %>% group_by(School.System.Name) %>%
  summarize(min_students = min(Total.Students),
            max_students = max(Total.Students))


#Find average number of students in each school system

la22 %>% group_by(School.System.Name) %>%
  summarize(avg_students = mean(Total.Students))


#Find the number of American Indian students and Black students in each school system

la22 %>% group_by(School.System.Name) %>%
  summarize(amind = sum(AmInd),
            black = sum(Black))

#You can imagine this would get really tedious if we had to do it for every race. In the next lesson, we'll learn how to rearrange the data in a way that allows us to avoid this problem.

#Find the number of Kindergarten, Grade 1 and Grade 2 students in each parish

la22 %>% group_by(School.System.Name) %>%
  summarize(kinder = sum(Kindergarten),
            first = sum(Grade1),
            second = sum(Grade2)) %>%
  arrange(-kinder) %>%
  View()



#Put it all together: select school name, parish code, kindergarten, Nonprofit and percent disadvantaged; filter for New Orleans; mutate to multiple percent_disadvantaged by 100; group_by nonprofit; summarize total kindergarten enrollment and median percent disadvantaged for each nonprofit group; arrange by kindergarten enrollment (largest on top)

kinder_summary = la22 %>% select(SiteName, Parish.Code, Kindergarten, Nonprofit, ED.) %>%
 filter(Parish.Code == 36) %>%
 mutate(pct_disadvantaged = ED. * 100) %>%
 group_by(Nonprofit) %>%
 summarize(total_kinder = sum(Kindergarten),
            median_ed = median(pct_disadvantaged),
           total_schools = n()) %>%
 arrange(-total_kinder) %>%
  ungroup()

#Challenge: Come up with your own code that incorporates each of the stupendous six functions

la22 %>% filter(str_detect(SiteName, "Detention") |
                str_detect(SiteName, "Correctional")) %>%
  select(SiteName, ED.)




# Joining and binding data ####
#Joining allows us to combine different datasets together.
#Louisiana awards all schools a letter grade and school performance score. Let's join that data to the enrollment data.

grades = read_excel("./data/2022-school-performance-scores.xlsx", skip=3) %>%
  slice(1:1174)

#Select first six columns for simplicity and clean up the column names
grades = grades %>% select(1:6)
colnames(grades) = c("site_code", "school", "school_system", "school_type", "grade", "sps")

#When you join datasets, the one you list first is on the left, and the one you list second is on the right.
#A left_join will keep all the rows on the left and drop rows on the right that don't have a match

merged = left_join(la22, grades, by=c("SiteCd"="site_code"))

#A right_join will do the reverse
merged = right_join(la22, grades, by=c("SiteCd"="site_code"))

#An inner_join will only keep the rows that both sides have in common
merged = inner_join(la22, grades, by=c("SiteCd"="site_code"))

#A full_join will keep all rows on both sides
merged = full_join(la22, grades, by=c("SiteCd"="site_code"))

#An anti_join will only keep the rows that both sides DON'T have in common
anti_join(grades, la22, by=c("site_code"="SiteCd"))

anti_join(la22, grades, by=c("SiteCd"="site_code"))



# Binding data
# You join data in order to bring in new columns that didn't exist in your first dataframe. However, what if you want to combine two dataframes with the same columns?

#The federal government has data on the number of 3-5-year-olds in special education and the number of 6-21-year-olds receiving special education services under the Individuals with Disabilities Education Act (IDEA).

idea_3_to_5 = read_excel("./data/2021-bchildcountandedenvironment-2.xlsx", skip=8) %>%
  slice(1:61)

idea_6_to_21 = read_excel("./data/2021-bchildcountandedenvironment-3.xlsx", skip=8) %>%
  slice(1:61)

#We want to combine these dataframes together. In this case, the two dataframes have the exact same columns. Therefore, we want to combine them by stacking them vertically, not horizontally. For that, we use Rbind.

#First, let's add a column to each dataframe to identify the age rage

#idea_3_to_5 = idea_3_to_5 %>% mutate(age_range = "3-5")

idea_3_to_5$age_range = "3-5"
idea_6_to_21$age_range = "6-21"

#Bind both dataframes together using rbind()

idea = rbind(idea_3_to_5, idea_6_to_21)

#Both dataframes must have the exact same columns for rbind() to work, although they don't have to be in the same order.

#In addition the state name we also want to know the postal code and FIPS code. I always keep a CSV saved that has the FIPS code for every state and territory.

fips = read_csv("./data/state_fips.csv")

#Join FIPS code and postal codes to the idea dataframe.

idea = left_join(idea, fips, by=c("State"="Name"))







