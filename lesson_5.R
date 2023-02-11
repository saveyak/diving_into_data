#Lesson 5: Extras
#This is stuff we will probably not get to in class, but I'm leaving it here as an extra lesson

# Scraping PDFs ####

#To use the tabulizer package, make sure Java is installed first. See https://stackoverflow.com/questions/70036429/having-issues-installing-tabulizer-package-in-r
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
#If you can't install Java, install the open-source OpenJDK from https://www.azul.com/downloads/?version=java-8-lts&package=jdk#download-openjdk.
#Make sure that you install OpenJDK Version 8, and that the architecture is x86 64-bit -- new versions will not work. See: https://github.com/ropensci/tabulizer/issues/147

#IMPORTANT: Must run this code BEFORE loading any packages to avoid Java memory errors
#See: https://stackoverflow.com/questions/34624002/r-error-java-lang-outofmemoryerror-java-heap-space
options(java.parameters = "-Xmx8000m")

library(tidyverse)
library(tabulizer)
#library(readr)
#library(readxl)
#library(wordstonumbers)


# Purpose of this exercise: scrape PDF data on private school enrollment by grade from five states (Delaware, Florida, Maryland, Nebraska, Nevada). Clean each dataframe so that grades are labeled in a similar  way, then bind all the dataframes together. I wrote this code for a story where I had to see how much private school enrollment had changed in various states.

big_df = data.frame()

#Delaware

# Note 1: Data shows enrollment counts for just Delaware students and for all students. I assume we want "all students" for private school enrollment.
# Note 2: Delaware did not break down data by grade for 2019.

filepath = "./data/private_enrollment/DE_fall2021_private_homeschool_enrollment.pdf"

pdf_scrape = extract_tables(file=filepath)

de = pdf_scrape[[5]] %>%
  as_tibble() %>%
  select(grade=V1, private_enrollment=V7) %>%
  slice(3:18)
de['year_by_fall_date'] = str_extract(filepath,"\\d{4}")

filepath = "./data/private_enrollment/DE_fall2020_private_homeschool_enrollment.pdf"

pdf_scrape = extract_tables(file=filepath)
df = pdf_scrape[[4]] %>% as_tibble() %>% select(grade=V1,V4) %>% slice(3:18)
df[ , 3:4] <- str_split_fixed(df$V4," ", 2)
df['year_by_fall_date'] = str_extract(filepath,"\\d{4}")
df = df %>% select(grade,private_enrollment=V2,year_by_fall_date)

de = rbind(de, df)

de$private_enrollment = str_replace(de$private_enrollment, ",", "") %>% as.numeric()
de$year_by_fall_date = as.numeric(de$year_by_fall_date)

#2019 does not break down data by grade. According to the 2019-20 nonpublic school report page. 53, there were 15,074 private school students (2,382 of them out-of-state residents).
de[nrow(de)+1,] = list("Total", 15074, 2019)

de['state'] = "DE"
de = de %>% mutate(grade_clean = case_when(
  grade=="Kindergarten" ~ "kindergarten",
  grade=="Total" ~ "k_12_total",
  str_detect(grade, "Ungraded Elem") ~ "ungraded_elementary",
  str_detect(grade, "Ungraded Sec") ~ "ungraded_secondary",
  TRUE ~ str_c("grade_", grade)
))

big_df = rbind(big_df, de)

# Florida


fl_clean = function (filepath, i) {
  pdf_scrape = extract_tables(file=filepath)
  df = pdf_scrape[[i]] %>% as_tibble() %>% select(grade=V1, private_enrollment=V2)
  df = df %>% filter(grade!="Pre-K")
  df$private_enrollment = str_replace(df$private_enrollment,",","")
  df$private_enrollment = as.numeric(df$private_enrollment)
  df[nrow(df)+1,] = list("k_12_total",sum(df$private_enrollment))
  df['year_by_fall_date'] = str_extract(filepath, "\\d{4}")

  return(df)
}

fl = fl_clean("./data/private_enrollment/FL_fall2019_private_enrollment.pdf", 1)
df = fl_clean("./data/private_enrollment/FL_fall2020_private_enrollment.pdf", 2)
fl = rbind(fl, df)
df = fl_clean("./data/private_enrollment/FL_fall2021_private_enrollment.pdf", 2)
fl = rbind(fl, df)

fl['state'] = "FL"
fl = fl %>% mutate(grade_clean = case_when(
  grade == "K" ~ "kindergarten",
  str_detect(grade,"^\\d") ~ str_extract(grade,"\\d{1,2}"),
  TRUE ~ grade
))
fl = fl %>% mutate(grade_clean = case_when(
  str_detect(grade_clean,"^\\d") ~ str_c("grade_", grade_clean),
  TRUE ~ grade_clean
))

big_df = rbind(big_df, fl)

# Maryland

#Need to grab two tables from each PDF. The first shows enrollment in private K-12 schools, the second in church-exempt schools. I am NOT counting nursery schools or publicly-funded nonpublic schools (there are schools for children with disabilities who are still considered public school students.)

md_clean = function (filepath) {

  print("scraping first table")

  #Scrape first table
  pdf_scrape = extract_tables(file=filepath, pages=7, method="stream")
  df = pdf_scrape[[1]] %>% as_tibble()
  colnames(df) = make.names(df[2,])
  df = df[3,]
  df = df %>% pivot_longer(2:ncol(df),"grade",values_to="private_enrollment") %>%
    mutate(type="private_k12_schools")
  df = df %>% mutate(private_enrollment=str_replace(private_enrollment,",",""))
  df$private_enrollment = as.numeric(df$private_enrollment)

  print("scraping second table")

  pdf_scrape = extract_tables(file=filepath, pages=9, method="stream")
  df2 = pdf_scrape[[1]] %>% as_tibble()
  df2 = df2 %>% select(-(2:5))
  colnames(df2) = make.names(df2[2,])
  df2 = df2[3,]
  df2 = df2 %>% pivot_longer(2:ncol(df2),"grade",values_to="private_enrollment") %>%
    mutate(type="church_exempt_schools")
  df2 = df2 %>% mutate(private_enrollment=str_replace(private_enrollment,",",""))
  df2$private_enrollment = as.numeric(df2$private_enrollment)
  df2[nrow(df2)+1,] = list("Total State", "Total",sum(df2$private_enrollment),"church_exempt_schools")

  df=rbind(df, df2)
  df = df %>% group_by(grade) %>% summarize(private_enrollment=sum(private_enrollment))
  df['year_by_fall_date'] = str_extract(filepath,"\\d{4}")

  return(df)

}

md_list = c("./data/private_enrollment/MD_fall2019_private_enrollment.pdf", "./data/private_enrollment/MD_fall2020_private_enrollment.pdf", "./data/private_enrollment/MD_fall2021_private_enrollment.pdf")

md = data.frame()

for (filepath in md_list) {
  df = md_clean(filepath)
  md = rbind(md, df)
}

md['state'] = "MD"
md = md %>% mutate(grade_clean = case_when(
  grade=="Kgn." ~ "kindergarten",
  grade=="Total" ~ "k_12_total",
  TRUE ~ str_replace(grade,"X","grade_")
))

big_df = rbind(big_df, md)


# Nebraska

ne_list = c("./data/private_enrollment/NE_fall2021_private_enrollment.pdf", "./data/private_enrollment/NE_fall2020_private_enrollment.pdf", "./data/private_enrollment/NE_fall2019_private_enrollment.pdf")

ne = data.frame()

for (filepath in ne_list) {

  print(filepath)
  pdf_scrape = extract_tables(file=filepath, pages=7, method="stream")
  df = pdf_scrape[[1]] %>% as_tibble()
  colnames(df) = make.names(df[4,])
  df = df %>% separate(X5.6, c("X5", "X6"), " ")
  df = df %>% filter(System == "NON PUBLIC") %>% select(4:16)
  df = df %>% pivot_longer(K:X12, "grade", values_to="private_enrollment")
  df$private_enrollment = str_replace(df$private_enrollment, ",","") %>% as.numeric()
  df[14,] = list("k_12_total",sum(df$private_enrollment))
  df['year_by_fall_date'] = str_extract(filepath, "\\d{4}")

  ne = rbind(ne, df)

}

ne['state'] = "NE"
ne = ne %>% mutate(grade_clean = case_when(
  grade=="K" ~ "kindergarten",
  str_detect(grade, "X") ~ str_replace(grade,"X","grade_"),
  TRUE ~ grade
))

big_df = rbind(big_df, ne)

# Nevada

pdf_scrape = extract_tables(file="./data/private_enrollment/NV_fall2021_private_enrollments.pdf", pages=4:5)

nv = data.frame()

df = pdf_scrape[[1]] %>% as_tibble()
colnames(df) = df[1,]
df = df[-1,]
df['year_by_fall_date'] = 2021
nv = rbind(nv, df)

df = pdf_scrape[[2]] %>% as_tibble()
colnames(df) = df[1,]
df = df[-1,]
df['year_by_fall_date'] = 2020
nv = rbind(nv, df)

df = pdf_scrape[[3]] %>% as_tibble()
colnames(df) = df[1,]
df = df[-1,]
df['year_by_fall_date'] = 2019
nv = rbind(nv, df)

nv = nv %>% select(grade=Grade, private_enrollment=Total, year_by_fall_date)
nv$private_enrollment = str_replace(nv$private_enrollment, ",", "") %>% as.numeric()
nv = nv %>% mutate(grade_clean = case_when(
  grade=="Totals" ~ "k_12_total",
  grade=="Kindergarten" ~ "kindergarten",
  TRUE ~ str_replace(grade, "Grade ", "grade_")
))
nv['state'] = "NV"

big_df = rbind(big_df, nv)

# Mapping using Census shapefiles ####

#Uncomment this to install the Associated Press official theme (optional)
#devtools::install_github("associatedpress/aptheme")

#Use install.packages() for any of the other following libraries if they're not already installed

library(tidyverse)
library(readr)
library(ggplot2)
library(sf)
library(tigris)
library(viridis)
library(aptheme)
library(rmapshaper)

# Mapping state-level changes from 2019 to 2022

# This data came from https://educationrecoveryscorecard.org/
# It measures learning loss in each state, as well as many school districts, before and after the COVID pandemic. Learning loss is measured by taking changes in test math and ELA test scores and converting them to grade-level equivalents, so a -1 means a one year's worth of learning was lost.
# I analyzed it for the article "Massive learning setbacks show COVID's sweeping toll on kids"
# https://apnews.com/article/health-education-covid-46cb725e08110f8ad3c1b303ec9eefad
# Open the file learning_loss_data_dictionary.xlsx to see notes on the data and understand how to read the columns of the dataframe

learning_loss = read_csv("./data/state_learning_loss_data.csv")

# Use the states() function from the Tigris library to grab Census shapefiles on every state in the US. Shift_geometry() moves Alaska and Hawaii on the map.
# A shapefile is a dataframe with geographic information that allows you to generate a map
# You can grab shapefiles from the Census for regions, counties, ZIP codes, Census blocks, even school districts. See https://github.com/walkerke/tigris for more

us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

us_states = left_join(us_states, learning_loss, by=c("GEOID"="id"))

reading_changes = ggplot(us_states) +
  geom_sf(aes(fill=all_ela1922), color=NA) +
  scale_fill_viridis() +
  labs(title="States with biggest changes in reading scores", subtitle="Change from 2019 to 2022", fill='Change in grade levels')

reading_changes

#If you want to customize the look more:
reading_changes +
  theme_ap() +
  theme(legend.position = 'right',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text())

#More info on AP theme here: https://github.com/associatedpress/aptheme
#To add state names: (goem_sf_label for boxes)
#geom_sf_text(data=subset(us_states, all_ela1922>0 | all_ela1922< -0.6), aes(label=STUSPS))

#geom_sf_text() if you don't want the boxes

#Map states with the biggest changes in reading/math scores for Black/Hispanic students
ggplot(us_states) +
  geom_sf(aes(fill=b_ela1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="States with biggest changes in reading scores for Black students")

ggplot(us_states) +
  geom_sf(aes(fill=h_ela1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="States with biggest changes in reading scores for Hispanic students")

ggplot(us_states) +
  geom_sf(aes(fill=all_mth1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="States with biggest changes in math scores")

ggplot(us_states) +
  geom_sf(aes(fill=b_mth1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="States with biggest changes in math scores for Black students")

ggplot(us_states) +
  geom_sf(aes(fill=h_mth1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="States with biggest changes in math scores for Hispanic students")

# Map changes at the district level

districts = read_csv("./data/district_learning_loss_data.csv")

#Mapping school districts is tricky. Some states have elementary districts, secondary districts and unified districts. The three types have to be joined together. There will still be a few blank spaces on the map where school districts are missing.
#In addition, these are huge shapefiles. So we need to use the function ms_simplify() from the mapshaper package to reduce the file size and avoid crashing your computer.

us_districts <- school_districts(cb = TRUE) %>%
  shift_geometry() %>% ms_simplify(keep = 0.001)

us_districts_elem <- school_districts(cb = TRUE, type="elementary") %>%
  ms_simplify(keep = 0.001)

us_districts_sec <- school_districts(cb = TRUE, type="secondary") %>%
  ms_simplify(keep = 0.001)

us_districts = left_join(us_districts, districts, by=c("GEOID"="id"))
us_districts_elem = left_join(us_districts_elem, districts, by=c("GEOID"="id"))
us_districts_sec = left_join(us_districts_sec, districts, by=c("GEOID"="id"))

ggplot() +
  geom_sf(data=us_districts, aes(fill=all_ela1922), color=NA) +
  geom_sf(data=us_districts_elem, aes(fill=all_ela1922), color=NA) +
  geom_sf(data=us_districts_sec, aes(fill=all_ela1922), color=NA) +
  scale_fill_viridis() +
  labs(title="Districts in the U.S. with biggest changes in reading scores") +
  theme_bw()

ggplot() +
  geom_sf(data=us_districts, aes(fill=all_mth1922), color=NA) +
  geom_sf(data=us_districts_elem, aes(fill=all_mth1922), color=NA) +
  geom_sf(data=us_districts_sec, aes(fill=all_mth1922), color=NA) +
  scale_fill_viridis() +
  labs(title="Districts in the U.S. with biggest changes in math scores") +
  theme_bw()

#Look at school districts in a specific state

#Alabama just has one type of school district so we don't have to worry about joining elementary/secondary/unified
al_districts <- school_districts(cb = TRUE, state="AL")

al_districts = left_join(al_districts, districts, by=c("GEOID"="id"))

df = al_districts %>% select(STATEFP:name, all_ela1922, all_ela1922_e, all_mth1922, all_mth1922_e, b_ela1922, b_ela1922_e, b_mth1922, b_mth1922_e, h_ela1922, h_ela1922_e, b_mth1922, b_mth1922_e, perfrl, pct_remote, esser_per_student)

ggplot(al_districts) +
  geom_sf(aes(fill=all_ela1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="Districts in Alabama with biggest changes in reading scores")

ggplot(al_districts) +
  geom_sf(aes(fill=b_ela1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="Districts with biggest changes in Black reading scores")

ggplot(al_districts) +
  geom_sf(aes(fill=h_ela1922), color=NA) +
  theme_void() +
  scale_fill_viridis() +
  labs(title="Districts in Alabama with biggest changes in reading scores for Hispanic students")

# Using R to create a custom map in Datawrapper

#Datawrapper.de is an amazing resource for creating professional-quality, interactive graphics for publication

#You can use the st_write() functions from the sf library to create a GeoJSON. That GeoJSON can then be uploaded to Datawrapper to create a customized interactive map, which you can then embed on a website.

st_write(al_districts, "al_districts.geojson")

#If the file size is too big to upload to Datawrapper, use ms_simplify(keep = 0.001) to reduce the file size

#More on simplifying with RMapshaper: https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html

#Instructions for Datawrapper:
# Save the data you want to put on the map as a CSV using write_csv()
# Go to Create New --> Map --> Choropleth Map.
# Click where it says "Upload Map" and upload al_districts.json.
# Click "match." Make sure that the matching key is AFFGEOID and the column selected is also AFFGEOID.
# Then select the column that has the variable you want to illustrate under Values. (E.g., all_ela1922.)
# Click "check" to make sure there are no rows that are causing errors.
# Click "proceed" and edit the map as you normally would. Here is the guide for creating choropleth maps on Datawrapper: https://academy.datawrapper.de/category/93-maps
# You will also want to update the tooltips so they show the name of the district instead of the GEOID. Here is a guide for customizing tooltips: https://academy.datawrapper.de/article/237-i-want-to-change-how-my-data-appears-in-tooltips



# Scraping websites ####

# Scraping a website allows us to programmatically collect the information we see on the page. It's the same as going through the website and writing everything manually into a spreadsheet, but we get a computer to do the task for us instead.
# There is no one-size-fits-all guide for scraping because every single website is different and therefore requires a different strategy. The following guide lists just a few simple examples.

# IMPORTANT: Make sure you're allowed to scrape a website before you do it, especially if it is a commercial site which may have proprietary data. Some websites consider scraping to be a violation of their Terms of Service. LinkedIn and Facebook have both taken legal actions against companies that scraped their data for commercial gain. I have not heard of a case where a journalist has been sued for web-scraping, but you could have your IP blocked.
# A scraper that makes too many requests of a web server within a very short time period could also be blocked because it might be seen as a hacking attempt
# Usually you can check if you're allowed to scrape a website by looking at a file called robots.txt. Read this guide to find out how: https://moz.com/learn/seo/robotstxt

#Method One for Scraping: Don't.

#Very frequently, websites hide their data behind the scenes. You can download their data directly by inspecting the webpage. It just takes some snooping to find out where it's hidden.
#Example 1:
#Go to https://edopportunity.org/recovery/#/map/
#Right click on the page and select "Inspect page" to see the inspection pane
#At the top of the inspection pane you'll see a header that says Elements, Console, Sources, Network, etc. Click on "Network" (if you don't see it, click the >> symbol to see more tabs.)
#Click on "Fetch/XHR." This will help you find what data sources the webpage is fetching to populate the map.
#Refresh the page.
#You'll see a bunch of links. One of them is "districts.csv". Double click and you'll automatically download a CSV with all the district data.

#It is not always so obvious where the data is hidden. Sometimes you have to click through a bunch of links to find the data. Often these links have the word "api" in them, which stands for application programming interface -- a way to automatically grab data from a website. Many public data sources like the Census website have APIs to help people make queries and receive data, and once you get more advanced at R, this is a great topic to study.
#Sometimes searching for "API" or "json" or "csv" in the box that says "filter" can help you find the link to click on. And sometimes there is no hidden data, sadly.
#Often the data is in a format called JSON. This format can be hard for humans to read. Thankfully, you can convert JSONs to regular dataframes using a package called jsonlite.

#Example 2:
#Get a list of members of the Georgia House of Representatives
#Go to https://www.legis.ga.gov/members/house (Note: I checked and there is no robots.txt for this website; since it is all public information, I'm confident we can scrape this page)
#Right click, hit inspect to open the inspection pane, click network, refresh
#Click on the link that says "1031?chamber=1"
#This link will open: https://www.legis.ga.gov/api/members/list/1031?chamber=1
#Right click and save as "georgia_house.json" in the data folder for this project.

#install.packages("jsonlite")
library(jsonlite)
house = fromJSON("./data/georgia_house.json")
View(house)
#Save as CSV (update name to reflect which session year you're scraping)
#to_csv(house,"georgia_house_2023_2024.csv")

#The Senate can be scraped the same way: https://www.legis.ga.gov/members/senate
#Read more about working with JSON files here: https://blog.exploratory.io/working-with-json-data-in-very-simple-way-ad7ebcc0bb89

# Method 2 for scraping: use rvest()

#Rvest (like "harvest," get it?) is yet another useful Tidyverse package that lets you scrape data.
#This webpage will give you an overview of web-scraping 101 with Rvest: https://rvest.tidyverse.org/articles/rvest.html

#Example scrape: Texas financial data

library(tidyverse)
library(rvest)

#We want to scrape the financial budget reports from Texas school districts, found here: https://rptsvr1.tea.texas.gov/school.finance/forecasting/financial_reports/2122_FinBudRep.html
#When you select a district, it takes you to a page like this: https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=sfadhoc.budget_report_2022.sas&_service=appserv&_debug=0&who_box=&who_list=101912

#Scrape one page as an example.
#This webpage simply consists of a table so it's easy to scrape with rvest.

url = "https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=sfadhoc.budget_report_2022.sas&_service=appserv&_debug=0&who_box=&who_list=101912"
webpage = read_html(url) #Read the HTML, the code that makes up the webpage

tables <- html_nodes(webpage, "table") %>% #Search for all "table" elements in the HTML
  html_table() #Turn those tables into dataframes
budget = tables[[2]] #The scraper found three tables; we only need the 2nd
View(budget)

#The table has rows that span multiple columns, which make the resulting output messy. I will leave it as an exercise to you to clean it up fully. For now I'll just fix the columns and also add a column with the district code.
colnames(budget) = c("category","gen_fund","gen_fund_pct", "gen_fund_per_student", "all_funds", "all_funds_percent", "all_funds_per_student")
budget$district_code = str_sub(url, start= -6) #Grab the last six digits of the URL

# Now do this for every Texas district.
#You will notice that the page for every district has a very similar URL. The only thing that changes is the district code.
#For example, here are Dallas and Brownsville:
#https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=sfadhoc.budget_report_2022.sas&_service=appserv&_debug=0&who_box=&who_list=057905
#https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=sfadhoc.budget_report_2022.sas&_service=appserv&_debug=0&who_box=&who_list=031901

# Assuming that you are able to find a list of all the districts and their codes, you should be able to repeat the same code for each district.

# I will do it for just three districts as an example. It's always a good idea to test your code on a small sample of districts before trying to scrape everything.

# IMPORTANT: If you want to actually scrape every districts in Texas, that's a huge amount of information, and may overload the servers. Use Sys.sleep(3) to make your code pause three seconds every time you scrape a page. It will make the code run more slowly so the website won't crash or block you.

#Write a custom function

tea_scrape = function(district_code) {

  url = paste0("https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=sfadhoc.budget_report_2022.sas&_service=appserv&_debug=0&who_box=&who_list=", district_code)
  print(paste0("scraping ", url))
  webpage = read_html(url)

  tables <- html_nodes(webpage, "table") %>%
    html_table()
  budget = tables[[2]]

  colnames(budget) = c("category","gen_fund","gen_fund_pct", "gen_fund_per_student", "all_funds", "all_funds_percent", "all_funds_per_student")
  budget$district_code = str_sub(url, start= -6)

  Sys.sleep(3) #Pause for three seconds to avoid overloading the server

  return(budget)
}


district_codes = c("101912", "057905", "031901")

all_budgets = data.frame()
count = 1

for (district_code in district_codes) {
  print(count)
  budget = tea_scrape(district_code)
  all_budgets = rbind(all_budgets, budget)
  count = count + 1
}

#Note: I added a counter to this because it will take a while for the code to run if you scrape all the Texas districts. The counter simply goes up by one every time the for loop runs, so that you can keep track of how many districts you've scraped so far.

View(all_budgets)

#write_csv(all_budgets, "texas_financial_reports.csv")
