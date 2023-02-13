#Diving into Data R Class Lesson 1: Exploring the Basics
#In this lesson, we will:
# - Discuss why a data journalist should learn R
# - Explore the layout of RStudio
# - Understand terminology like object, vector, dataframe and function
# - Install and load packages
# - Create dataframes from CSV, Excel or manually
# - Explore a dataframe and select columns/rows
# - Install and load packages
# - Introduction to the Tidyverse

# Introduction ####
# Why use R -- see Intro to R powerpoint

# Let's have a tour of the RStudio environment:
# Upper right corner: environment shows objects you've created
# Lower right corner: files show your R scripts, folders and data files; plots shows graphs you've made; packages shows all the packages you have; type ?? and a search term for help to appear in the "help" tab
# Below: The console is where the output of your code appears

# Basic terminology: object, string, function, vector, dataframe ####
#Press CTRL + Enter to run a line of code
# An object is a way to store information
#You give it a name, then assign it a value using the assignment operator
#The assignment operator can be = or <-

# THIS IS A COMMENT

my_age = 100
my_age_in_dog_years = my_age*7 #Multiply your age by 7



#Can also be written this way:
#my_age <- 31
#my_age_in_dog_years <- my_age * 7

my_age_in_dog_years
#Go up and edit my_age to a different number. What happens to my_age_in_dog_years?

#You can do whatever math you want. Addition, subtraction, etc. Run this code or type it in the console.
what_is_100_times_7 = 100*7
81/7
81%%7 #This is a modulo -- gives you the remainder of division
8+8

#Woohoo, it's a calculator. Who cares? Let's have some words too!

my_first_name = "Sharon"
my_last_name = "Lurye"

#Replace this with your own name
#You can also write <- instead of =

#Notice both words are in quotation marks. A set of characters is called a string and must be put in quotation marks.

#A function is like a verb. It does something to whatever input you put into the parentheses. Example: toupper() is a function that will make a string all uppercase.

toupper(my_first_name)

#Guess what you need to do to write your first name in lowercase.

tolower(my_first_name)


#A vector is a list of multiple things. It always starts with the letter "c" for "combined."
first_names = c("Ty", "Sami", "Mikhail", "Mirtha", "Camille")

first_names

last_names = c("Tagami", "Edge", "Zinshteyn", "Donastorg", "Phillips")
organizations = c("The Atlanta Journal-Constitution", "The Oregonian", "Calmatters", "The Plug", "Texas Public Radio")

states = c("Georgia", "Oregon", "California", "Georgia", "Texas")


ages = c(105, 7, 62, 33, 41.5)
zips = c(30030, 97202, 90036, 30307, 78222)

mac_users = c(FALSE, TRUE, TRUE, TRUE, FALSE)

#Notice the last vector is TRUE or FALSE. That's what's known as a logical. More on that later.

#You can grab the first item in a vector by the writing the name of the name of the vector followed by [1]
first_names[1]

first_names[2]

#The second item is [2], third is [3], etc.
ages[2]

#If a list is too long to count, use the function length()
length(states)

#Grab the third item on the list of organizations

organizations[3]




#Grab the last item on the list of ages
ages[5]



#You probably put age[5]. What if you weren't sure how many items were in the list? Is there another way to get the last item in the list using length()?

#Concise way
ages[length(ages)]

#Less concise way
#How many items are on the list?

number_of_items_on_list = length(ages)
ages[number_of_items_on_list]




#You can put multiple vectors together into a dataframe, which is basically a spreadsheet.

#data.frame()

df = data.frame(first = first_names,
                last = last_names,
                org = organizations,
                state = states,
                age = ages,
                zip_code = zips,
                uses_mac = mac_users)

#I don't want to look at this dataframe in the console. Let's take a closer View()! You can also click on "df" in th environment pane.

View(df)

#You can select a column by typing the name of the dataframe, then $, then the column name
df$last

#You can also select by number. The format is: df[rownumber, columnnumber]
#Example: Row 1, Column 2
df[1,2]

#Everything in row 1
df[1,] #Notice there's a comma, then the column number is blank

#There are four ways to get everything in column 3:
df[,3]
df[3]
df['org']
df$org

#To get everything EXCEPT column 3:
df[-3]
#Everything EXCEPT row 1:
df[-1,]



#Give me everything in column 4
df$state
df[4]


#Give me row 2, column 2
df[2,2]


#Give me everything in row 5
df[5,]


#To find out the number of rows, use the function nrow() and ncol()

nrow(df)
ncol(df)

#Give me everything in the last row, using nrow()

df[nrow(df),]

#Give me everything EXCEPT the last row

df[-nrow(df),]
df[-5,]

#To grab multiple columns, you can put in a number range:
df[2:4]

#What if you need to grab columns 1, 2, and 5?
df[c(1:2, 5)]
df[c('first', 'last', 'age')]

#Let's have a summary of this dataframe
summary(df)

#Wait, it doesn't make much sense for the Zip Code to have quartiles! It's not really a number. We should have put quotation marks around the zip codes to make them strings.
#Use the as.character() function to change the numbers to a string

as.character(df$zip_code)

#Let's see if that worked

summary(df)

#Wait, what happened? Nothing changed. Let's try this code instead:

df$zip_code = as.character(df$zip_code)
#Explanation: we need the assignment operator = or <- (both are exactly the same)
#df$zip_code = -----> Specifies that I'm going to change the "zip_code" column. If I don't have this part of the code at the beginning of the line, I'll see an output from my function, but nothing will happen to the dataframe!
#It's a very common mistake to forget the assignment operator. It's like doing a bunch of things in Excel and then forgetting to save your work.
#However, you can leave out the assignment operator on purpose if you're just messing around/testing code and don't want to actually save your changes.


#Find the working directory and read CSVs ####

#Creating a data.frame by typing it out manually seems extremely tedious. Isn't there a better way? Yes, you can also read CSVs and Excel spreadsheets and load them directly into dataframes.

#First, though, you have to figure out: where I am? Find the working directory.
getwd()

#Any time you want to load a data file, you have to give directions in reference to the working directory.
#My working directory is "/Users/slurye/data_projects/diving_into_data"
#If for whatever reason it wasn't, I would use this code:
#setwd("/Users/slurye/data_projects/diving_into_data")
#Alternatively: Go to session --> set working directory

#This code should work if your working directory is in the right place
#Display all the files in the data folder:
list.files("./data/")
#Display all the files in the sc_teacher_demographics folder:
list.files("./data/sc_teacher_demographics")

#To read a CSV, use the function read.csv()
construction_csv = read.csv(file="./data/district_MO_construction_SY15_to_SY19.csv")
View(construction_csv)

#Problem: There's several rows at the top we don't need. Tell R to skip the first 6 lines; the 6th line then automatically becomes the header.

construction_csv = read.csv(file="./data/district_MO_construction_SY15_to_SY19.csv", skip=6)
View(construction_csv)

#This is a file that shows how much school districts spend on maintenance and construction from 2015 to 2019. We'll work more with it later.

#R can read Excel spreadsheets too but first we need to learn about installing packages.

#### Installing packages ####
#R is super awesome but it could be better. Because it's open source, anyone can create a new package that improves on base R and lets users do even more cool stuff.
#The most important package to install is the tidyverse. I use it so often that I can't imagine R without it.
#Tidyverse actually a collection of packages that are all very useful, including:
#ggplot2 --> create graphics
#dplyr --> helps you manipulate data
#tidyr --> lets you pivot dataframes from wide to long or vice-versa
#readr --> read CSVs
#readxl --> read Excel spreadsheets
#stringr --> clean strings


#Uncomment below to install:
#install.packages("tidyverse")

#You only have to install.packages() once. However, you need to go to the library() to load them every time you run a new script.
library(tidyverse)
library(readxl)

#Let's open an Excel sheet in the the sc_teacher_demographics folder. This spreadsheet, which I got from the South Carolina Department of Education website, shows the number of South Carolina public school teachers by race and gender in the 2020-21 school year.

sc21 = read_excel("./data/sc_teacher_demographics/SOUTH CAROLINA TEACHERS BY RACE AND GENDER BY SCHOOL DISTRICT (PRELIMINARY REPORT), 2020-21.xlsx")

#If this is a pain to type out, remember that you can copy and paste the name of the Excel spreadsheet from the list of files. How do you get the list of files in sc_teacher_demographics again?

list.files("./data/sc_teacher_demographics/")





#The tidyverse has a nifty function called glimpse() that lets us quickly see all of our columns and what type they are
glimpse(sc21)

summary(sc21$`WHITE MALES`)

#Dbl stands for double, which is a non-integer number (i.e. a number with decimal points). Int stands for integer, chr for character, lgl for logical. Another important type is a factor which we will explain later.

#Open up sc21 to view in another tab.
view(sc21)




#There are some useful notes at the bottom. You could write these down wherever you're keeping the notes for your story, or write the note as a comment. But, we don't need them for our analysis, so we're going to get rid of them.
#drop_na() will remove all rows that contain *any* NA values.

sc21 = drop_na(sc21)


#Let's say we want a column with the total number of white teachers. We will call this column total_whites.
#We can make this column simply by using plus signs to add the three columns that count white teachers.

sc21$total_whites = sc21$`WHITE MALES` + sc21$`WHITE FEMALES` + sc21$`WHITE GENDER NOT REPORTED`

#Notice that the column name has to be inside tick marks (NOT a regular quotation mark!). This is because there are spaces in the column name. Later on we will learn how to clean column names so we don't have to deal with this annoying extra step.

#Now calculate the percentage of teachers who are white. Divide the number of white teachers by the total number of teachers then multiply by 100.
sc21$pct_white = sc21$total_whites / sc21$`TOTAL NUMBER OF TEACHERS` * 100

#Calculate percent (race/gender) for another race or gender.








#The code above is laborious to type. Tidyverse introduces the pipe operator, which looks like this: %>%
#Think of a pipe as saying "and then do this" -- it allows you to combine multiple commands in order.
#This code does the exact same thing as above, but in far fewer lines.
#It uses a dplyr function called mutate() which creates new columns -- we will learn more about this soon.

sc21 = read_excel("./data/sc_teacher_demographics/SOUTH CAROLINA TEACHERS BY RACE AND GENDER BY SCHOOL DISTRICT (PRELIMINARY REPORT), 2020-21.xlsx") %>%
  drop_na() %>%
  mutate(total_whites = `WHITE MALES` + `WHITE FEMALES` + `WHITE GENDER NOT REPORTED`,
         pct_white = total_whites/`TOTAL NUMBER OF TEACHERS`*100)

view(sc21)

sc21 %>% View() #Same as View(sc21)

#Tidyverse also allows us to grab rows using the slice() function. Examples: grab rows 2 to 5
#Base R way
sc21[2:5,]

#Tidyverse way
sc21 %>% slice(2:5)

#It's not really shorter, but it's easier to read and understand.

#Grab the first 10 rows
sc21 %>% slice_head(n=10)
#Guess how to grab the last 10 rows

bottom_10 = sc21 %>% slice_tail(n=10)

#Grab a random sample of 10 rows
sc21 %>% slice_sample(n=10)

view(mtcars)

#Grab the 25% of rows that have the lowest share of white teachers
sc21 %>% slice_min(pct_white, prop=0.25) %>% View()

#Guess how to do the same thing for the 25% of rows with the largest share of white teachers.
sc21 %>% slice_max(pct_white, prop=0.25) %>% View()



#We will be using the %>% pipe and tidyverse functions extensively in the next lesson, where we will learn how to manipulate data using the functions filter(), mutate(), select(), summarize(), group_by() and arrange().

