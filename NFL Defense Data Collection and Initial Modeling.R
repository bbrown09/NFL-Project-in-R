library(tidyverse)
library(rvest)
library(janitor)
library(xml2)
library(sqldf)
library(ggfortify)
library(car)
library(readxl)
library(corrplot)
library(randomForest)
library(gbm)

#pro football data collection 2010-2020

pro_football_reference_2020_url = read_html("https://www.pro-football-reference.com/years/2020/opp.htm")
pff_data_2020 = pro_football_reference_2020_url %>%
  html_nodes("table") %>%
  html_table()

passing2020 = pro_football_reference_2020_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2020 = passing2020 %>%
  add_column(Year = "2020")

rushing2020 = pro_football_reference_2020_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2020 = rushing2020 %>%
  add_column(Year = "2020")

conversions2020 = pro_football_reference_2020_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2020 = janitor::row_to_names(conversions2020, 1)
conversions2020 = conversions2020 %>%
  add_column(Year = "2020")

pffdefense2020 = as.data.frame(pff_data_2020[1])
pffdefense2020 = janitor::row_to_names(pffdefense2020, 1)
pffdefense2020 = pffdefense2020 %>%
  add_column(Year = "2020")
pffadvanced2020 = as.data.frame(pff_data_2020[2])

#pff2019

pro_football_reference_2019_url = read_html("https://www.pro-football-reference.com/years/2019/opp.htm")
pff_data_2019 = pro_football_reference_2019_url %>%
  html_nodes("table") %>%
  html_table()

passing2019 = pro_football_reference_2019_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2019 = passing2019 %>%
  add_column(Year = "2019")

rushing2019 = pro_football_reference_2019_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2019 = rushing2019 %>%
  add_column(Year = "2019")

conversions2019 = pro_football_reference_2019_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2019 = janitor::row_to_names(conversions2019, 1)
conversions2019 = conversions2019 %>%
  add_column(Year = "2019")

pffdefense2019 = as.data.frame(pff_data_2019[1])
pffdefense2019 = janitor::row_to_names(pffdefense2019, 1)
pffdefense2019 = pffdefense2019 %>%
  add_column(Year = "2019")
pffadvanced2019 = as.data.frame(pff_data_2019[2])

#pff 2018

pro_football_reference_2018_url = read_html("https://www.pro-football-reference.com/years/2018/opp.htm")
pff_data_2018 = pro_football_reference_2018_url %>%
  html_nodes("table") %>%
  html_table()

passing2018 = pro_football_reference_2018_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2018 = passing2018 %>%
  add_column(Year = "2018")

rushing2018 = pro_football_reference_2018_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2018 = rushing2018 %>%
  add_column(Year = "2018")

conversions2018 = pro_football_reference_2018_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2018 = janitor::row_to_names(conversions2018, 1)
conversions2018 = conversions2018 %>%
  add_column(Year = "2018")

pffdefense2018 = as.data.frame(pff_data_2018[1])
pffdefense2018 = janitor::row_to_names(pffdefense2018, 1)
pffdefense2018 = pffdefense2018 %>%
  add_column(Year = "2018")
pffadvanced2018 = as.data.frame(pff_data_2018[2])

#pff 2017

pro_football_reference_2017_url = read_html("https://www.pro-football-reference.com/years/2017/opp.htm")
pff_data_2017 = pro_football_reference_2017_url %>%
  html_nodes("table") %>%
  html_table()

passing2017 = pro_football_reference_2017_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2017 = passing2017 %>%
  add_column(Year = "2017")

rushing2017 = pro_football_reference_2017_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2017 = rushing2017 %>%
  add_column(Year = "2017")

conversions2017 = pro_football_reference_2017_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2017 = janitor::row_to_names(conversions2017, 1)
conversions2017 = conversions2017 %>%
  add_column(Year = "2017")

pffdefense2017 = as.data.frame(pff_data_2017[1])
pffdefense2017 = janitor::row_to_names(pffdefense2017, 1)
pffdefense2017 = pffdefense2017 %>%
  add_column(Year = "2017")

#pff 2016

pro_football_reference_2016_url = read_html("https://www.pro-football-reference.com/years/2016/opp.htm")
pff_data_2016 = pro_football_reference_2016_url %>%
  html_nodes("table") %>%
  html_table()

passing2016 = pro_football_reference_2016_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2016 = passing2016 %>%
  add_column(Year = "2016")

rushing2016 = pro_football_reference_2016_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2016 = rushing2016 %>%
  add_column(Year = "2016")

conversions2016 = pro_football_reference_2016_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2016 = janitor::row_to_names(conversions2016, 1)
conversions2016 = conversions2016 %>%
  add_column(Year = "2016")

pffdefense2016 = as.data.frame(pff_data_2016[1])
pffdefense2016 = janitor::row_to_names(pffdefense2016, 1)
pffdefense2016 = pffdefense2016 %>%
  add_column(Year = "2016")

#pff 2015

pro_football_reference_2015_url = read_html("https://www.pro-football-reference.com/years/2015/opp.htm")
pff_data_2015 = pro_football_reference_2015_url %>%
  html_nodes("table") %>%
  html_table()

passing2015 = pro_football_reference_2015_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2015 = passing2015 %>%
  add_column(Year = "2015")

rushing2015 = pro_football_reference_2015_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2015 = rushing2015 %>%
  add_column(Year = "2015")

conversions2015 = pro_football_reference_2015_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2015 = janitor::row_to_names(conversions2015, 1)
conversions2015 = conversions2015 %>%
  add_column(Year = "2015")

pffdefense2015 = as.data.frame(pff_data_2015[1])
pffdefense2015 = janitor::row_to_names(pffdefense2015, 1)
pffdefense2015 = pffdefense2015 %>%
  add_column(Year = "2015")

#pff 2014

pro_football_reference_2014_url = read_html("https://www.pro-football-reference.com/years/2014/opp.htm")
pff_data_2014 = pro_football_reference_2014_url %>%
  html_nodes("table") %>%
  html_table()

passing2014 = pro_football_reference_2014_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2014 = passing2014 %>%
  add_column(Year = "2014")

rushing2014 = pro_football_reference_2014_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2014 = rushing2014 %>%
  add_column(Year = "2014")

conversions2014 = pro_football_reference_2014_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2014 = janitor::row_to_names(conversions2014, 1)
conversions2014 = conversions2014 %>%
  add_column(Year = "2014")

pffdefense2014 = as.data.frame(pff_data_2014[1])
pffdefense2014 = janitor::row_to_names(pffdefense2014, 1)
pffdefense2014 = pffdefense2014 %>%
  add_column(Year = "2014")

# pff 2013

pro_football_reference_2013_url = read_html("https://www.pro-football-reference.com/years/2013/opp.htm")
pff_data_2013 = pro_football_reference_2013_url %>%
  html_nodes("table") %>%
  html_table()

passing2013 = pro_football_reference_2013_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2013 = passing2013 %>%
  add_column(Year = "2013")

rushing2013 = pro_football_reference_2013_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2013 = rushing2013 %>%
  add_column(Year = "2013")

conversions2013 = pro_football_reference_2013_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2013 = janitor::row_to_names(conversions2013, 1)
conversions2013 = conversions2013 %>%
  add_column(Year = "2013")

pffdefense2013 = as.data.frame(pff_data_2013[1])
pffdefense2013 = janitor::row_to_names(pffdefense2013, 1)
pffdefense2013 = pffdefense2013 %>%
  add_column(Year = "2013")

# pff 2012

pro_football_reference_2012_url = read_html("https://www.pro-football-reference.com/years/2012/opp.htm")
pff_data_2012 = pro_football_reference_2012_url %>%
  html_nodes("table") %>%
  html_table()

passing2012 = pro_football_reference_2012_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2012 = passing2012 %>%
  add_column(Year = "2012")

rushing2012 = pro_football_reference_2012_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2012 = rushing2012 %>%
  add_column(Year = "2012")

conversions2012 = pro_football_reference_2012_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2012 = janitor::row_to_names(conversions2012, 1)
conversions2012 = conversions2012 %>%
  add_column(Year = "2012")

pffdefense2012 = as.data.frame(pff_data_2012[1])
pffdefense2012 = janitor::row_to_names(pffdefense2012, 1)
pffdefense2012 = pffdefense2012 %>%
  add_column(Year = "2012")

# pff 2011

pro_football_reference_2011_url = read_html("https://www.pro-football-reference.com/years/2011/opp.htm")
pff_data_2011 = pro_football_reference_2011_url %>%
  html_nodes("table") %>%
  html_table()

passing2011 = pro_football_reference_2011_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2011 = passing2011 %>%
  add_column(Year = "2011")

rushing2011 = pro_football_reference_2011_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2011 = rushing2011 %>%
  add_column(Year = "2011")

conversions2011 = pro_football_reference_2011_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2011 = janitor::row_to_names(conversions2011, 1)
conversions2011 = conversions2011 %>%
  add_column(Year = "2011")

pffdefense2011 = as.data.frame(pff_data_2011[1])
pffdefense2011 = janitor::row_to_names(pffdefense2011, 1)
pffdefense2011 = pffdefense2011 %>%
  add_column(Year = "2011")

# pff 2010

pro_football_reference_2010_url = read_html("https://www.pro-football-reference.com/years/2010/opp.htm")
pff_data_2010 = pro_football_reference_2010_url %>%
  html_nodes("table") %>%
  html_table()

passing2010 = pro_football_reference_2010_url %>% html_nodes(xpath = '//*[@id="all_passing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#passing') %>%   
  html_table()
passing2010 = passing2010 %>%
  add_column(Year = "2010")

rushing2010 = pro_football_reference_2010_url %>% html_nodes(xpath = '//*[@id="all_rushing"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#rushing') %>%   
  html_table()
rushing2010 = rushing2010 %>%
  add_column(Year = "2010")

conversions2010 = pro_football_reference_2010_url %>% html_nodes(xpath = '//*[@id="all_team_conversions"]/comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%   
  read_html() %>%  
  html_node('#team_conversions') %>%   
  html_table()
conversions2010 = janitor::row_to_names(conversions2010, 1)
conversions2010 = conversions2010 %>%
  add_column(Year = "2010")

pffdefense2010 = as.data.frame(pff_data_2010[1])
pffdefense2010 = janitor::row_to_names(pffdefense2010, 1)
pffdefense2010 = pffdefense2010 %>%
  add_column(Year = "2010")

# potential blitz data
football_outstiders_2019_url = "https://www.footballoutsiders.com/stat-analysis/2020/defense-number-pass-rushers-2019"
full_data_2019 = read_html(football_outstiders_2019_url) %>%
  html_nodes("table") %>%
  html_table()

three_or_less_2019 = as.data.frame(full_data_2019[1])
three_or_less_2019 = janitor::row_to_names(three_or_less_2019, 2)

four_or_more_2019 = as.data.frame(full_data_2019[2])
four_or_more_2019 = janitor::row_to_names(four_or_more_2019, 2)

five_or_more_2019 = as.data.frame(full_data_2019[3])
five_or_more_2019 = janitor::row_to_names(five_or_more_2019, 3)


db_blitz_2019 = as.data.frame(full_data_2019[4])
db_blitz_2019 = janitor::row_to_names(db_blitz_2019, 2)

football_outstiders_2018_url = "https://www.footballoutsiders.com/stat-analysis/2019/pressure-number-pass-rushers-2018"
full_data_2018 = read_html(football_outstiders_2018_url) %>%
  html_nodes("table") %>%
  html_table()

three_or_less_2018 = as.data.frame(full_data_2018[3])
three_or_less_2018 = janitor::row_to_names(three_or_less_2018, 2)

four_or_more_2018 = as.data.frame(full_data_2018[1])
four_or_more_2018 = janitor::row_to_names(four_or_more_2018, 2)

five_or_more_2018 = as.data.frame(full_data_2018[2])
five_or_more_2018 = janitor::row_to_names(five_or_more_2018, 2)

db_blitz_2018 = as.data.frame(full_data_2018[4])
db_blitz_2018 = janitor::row_to_names(db_blitz_2018, 2)

football_outstiders_2017_url = "https://www.footballoutsiders.com/stat-analysis/2018/pressure-number-pass-rushers-2017"
full_data_2017 = read_html(football_outstiders_2017_url) %>%
  html_nodes("table") %>%
  html_table()

three_or_less_2017 = as.data.frame(full_data_2017[3])
three_or_less_2017 = janitor::row_to_names(three_or_less_2017, 2)
three_or_less_2017 = subset(three_or_less_2017, select = -c(3,5,7))

four_or_more_2017 = as.data.frame(full_data_2017[1])
four_or_more_2017 = janitor::row_to_names(four_or_more_2017, 2)
four_or_more_2017 = subset(four_or_more_2017, select = -c(3,5,7))

five_or_more_2017 = as.data.frame(full_data_2017[2])
five_or_more_2017 = janitor::row_to_names(five_or_more_2017, 2)
five_or_more_2017 = subset(five_or_more_2017, select = -c(3,5,7))

db_blitz_2017 = as.data.frame(full_data_2017[4])
db_blitz_2017 = janitor::row_to_names(db_blitz_2017, 2)
db_blitz_2017 = subset(db_blitz_2017, select = -c(3,5,7))

football_outstiders_2016_url = "https://www.footballoutsiders.com/stat-analysis/2017/defense-and-pass-pressure-2016"
full_data_2016 = read_html(football_outstiders_2016_url) %>%
  html_nodes("table") %>%
  html_table()
with_and_without_pressure_2016 = as.data.frame(full_data_2016[1])
with_and_without_pressure_2016 = janitor::row_to_names(with_and_without_pressure_2016,2)
number_of_rushers_2016 = as.data.frame(full_data_2016[2])
number_of_rushers_2016 = janitor::row_to_names(number_of_rushers_2016, 2)

# cleaning

all_passing = rbind(passing2020, passing2019, passing2018, passing2017, passing2016,
                    passing2015, passing2014, passing2013, passing2012, passing2011, passing2010)
all_passing = all_passing[complete.cases(all_passing), ]
names(all_passing)[c(4:25)] = c("ps Cmp", "ps Att",	"ps Cmp%",	"ps Yds", "ps TD", "ps TD%",	"ps Int", "ps PD",	"ps Int%",
                                  "ps Y/A",	"ps AY/A",	"ps Y/C",	"ps Y/G",	"ps Rate",	"ps Sk",	"sk Yds",	"ps QBHits",
                                "ps TFL",	"ps Sk%",	"ps NY/A",	"ps ANY/A",	"ps EXP")
all_passing = select(all_passing, -1, -3)

all_rushing = rbind(rushing2020, rushing2019, rushing2018, rushing2017, rushing2016,
                    rushing2015, rushing2014, rushing2013, rushing2012, rushing2011, rushing2010)
all_rushing = all_rushing[complete.cases(all_rushing), ]
names(all_rushing)[c(4:9)] = c("rs Att",	"rs Yds", "rs TD", "rs Y/A",	"rs Y/G",	"rs EXP")
all_rushing = select(all_rushing, -1, -3)

all_conversions = rbind(conversions2020, conversions2019, conversions2018, conversions2017, conversions2016,
                    conversions2015, conversions2014, conversions2013, conversions2012, conversions2011, conversions2010)
all_conversions = all_conversions[!(is.na(all_conversions$Rk) | all_conversions$Rk==""), ]
all_conversions = select(all_conversions, -1, -3)

all_pffdefense = rbind(pffdefense2020, pffdefense2019, pffdefense2018, pffdefense2017, pffdefense2016,
                    pffdefense2015, pffdefense2014, pffdefense2013, pffdefense2012, pffdefense2011, pffdefense2010)
all_pffdefense = all_pffdefense[!(is.na(all_pffdefense$Rk) | all_pffdefense$Rk==""), ]
all_pffdefense = all_pffdefense[ -c(11:22) ]
names(all_pffdefense)[c(4:12)] = c("tot PA",	"tot Yds",	"tot Ply",	"tot Y/P",	"tot TO",	"tot FL",	"tot 1stD", "tot Pen", "pen Yds")
all_pffdefense = select(all_pffdefense, -1, -3)

#updating team names
all_passing$Tm = ifelse(all_passing$Tm=="San Diego Chargers", "Los Angeles Chargers", all_passing$Tm)
all_passing$Tm = ifelse(all_passing$Tm=="St. Louis Rams", "Los Angeles Rams", all_passing$Tm)
all_passing$Tm = ifelse(all_passing$Tm=="Washington Redskins", "Washington Football Team", all_passing$Tm)
all_passing$Tm = ifelse(all_passing$Tm=="Oakland Raiders", "Las Vegas Raiders", all_passing$Tm)

all_rushing$Tm = ifelse(all_rushing$Tm=="San Diego Chargers", "Los Angeles Chargers", all_rushing$Tm)
all_rushing$Tm = ifelse(all_rushing$Tm=="St. Louis Rams", "Los Angeles Rams", all_rushing$Tm)
all_rushing$Tm = ifelse(all_rushing$Tm=="Washington Redskins", "Washington Football Team", all_rushing$Tm)
all_rushing$Tm = ifelse(all_rushing$Tm=="Oakland Raiders", "Las Vegas Raiders", all_rushing$Tm)


all_conversions$Tm = ifelse(all_conversions$Tm=="San Diego Chargers", "Los Angeles Chargers", all_conversions$Tm)
all_conversions$Tm = ifelse(all_conversions$Tm=="St. Louis Rams", "Los Angeles Rams", all_conversions$Tm)
all_conversions$Tm = ifelse(all_conversions$Tm=="Washington Redskins", "Washington Football Team", all_conversions$Tm)
all_conversions$Tm = ifelse(all_conversions$Tm=="Oakland Raiders", "Las Vegas Raiders", all_conversions$Tm)


all_pffdefense$Tm = ifelse(all_pffdefense$Tm=="San Diego Chargers", "Los Angeles Chargers", all_pffdefense$Tm)
all_pffdefense$Tm = ifelse(all_pffdefense$Tm=="St. Louis Rams", "Los Angeles Rams", all_pffdefense$Tm)
all_pffdefense$Tm = ifelse(all_pffdefense$Tm=="Washington Redskins", "Washington Football Team", all_pffdefense$Tm)
all_pffdefense$Tm = ifelse(all_pffdefense$Tm=="Oakland Raiders", "Las Vegas Raiders", all_pffdefense$Tm)



#combining pff data
test = merge(all_passing, all_rushing)
test2 = merge(all_conversions, test)
allpffdata = merge(all_pffdefense, test2)

write.csv(allpffdata, "allpffdata.csv", row.names = FALSE)
rm(list = ls())

#nflfastR data
seasons = 2010:2020
pbp = map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(rush == 1 | pass == 1, week <= 17, !is.na(epa), !is.na(posteam), posteam != "") %>%
    select(season, posteam, pass, defteam, epa)
})

defense_epa = pbp %>%
  group_by(defteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(def_pass_epa = `1`, def_rush_epa = `0`)

#cleaning epa data

names(defense_epa)[c(1:4)] = c("Tm", "Year", "dr epa", "dp epa")

defense_epa$Tm = ifelse(defense_epa$Tm=="ARI", "Arizona Cardinals", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="ATL", "Atlanta Falcons", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="BAL", "Baltimore Ravens", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="BUF", "Buffalo Bills", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="CAR", "Carolina Panthers", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="CHI", "Chicago Bears", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="CIN", "Cincinnati Bengals", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="CLE", "Cleveland Browns", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="DAL", "Dallas Cowboys", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="DEN", "Denver Broncos", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="DET", "Detroit Lions", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="GB", "Green Bay Packers", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="HOU", "Houston Texans", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="IND", "Indianapolis Colts", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="JAX", "Jacksonville Jaguars", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="KC", "Kansas City Chiefs", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="MIA", "Miami Dolphins", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="MIN", "Minnesota Vikings", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="NE", "New England Patriots", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="NO", "New Orleans Saints", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="NYG", "New York Giants", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="NYJ", "New York Jets", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="LV", "Las Vegas Raiders", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="PHI", "Philadelphia Eagles", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="PIT", "Pittsburgh Steelers", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="LAC", "Los Angeles Chargers", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="SF", "San Francisco 49ers", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="SEA", "Seattle Seahawks", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="LA", "Los Angeles Rams", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="TB", "Tampa Bay Buccaneers", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="TEN", "Tennessee Titans", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="WAS", "Washington Football Team", defense_epa$Tm)
defense_epa$Tm = ifelse(defense_epa$Tm=="OAK", "Las Vegas Raiders", defense_epa$Tm)


# merging data

allpffdata = read.csv("allpffdata.csv")

epa_and_pff = merge(allpffdata, defense_epa)

for (i in 1:nrow(epa_and_pff)) {
  epa_and_pff$'combined epa'[i] = epa_and_pff$`dp epa`[i] + epa_and_pff$`dr epa`[i]
}

epa_and_pff$RZPct = as.numeric(gsub("[\\%,]", "", epa_and_pff$RZPct))
epa_and_pff$X3D. = as.numeric(gsub("[\\%,]", "", epa_and_pff$X3D.))
epa_and_pff$X4D. = as.numeric(gsub("[\\%,]", "", epa_and_pff$X4D.))



write.csv(epa_and_pff, "master.csv", row.names = FALSE)
rm(list = ls())

# exploratory data analysis
master = read.csv("master.csv")
master[] = lapply(master, function(x) {
  if(is.integer(x)) as.numeric(as.character(x)) else x
})

data.frame(colnames(master))

# collinearity check
sapply(master, class)
comb = master[c(3:30, 32:46, 49:52, 55)]
corrplot(cor(comb))
cor(comb)

summary(comb)


nfl_defense_lm = lm(combined.epa ~., data = comb)
summary(nfl_defense_lm)

comb$residuals = nfl_defense_lm$residuals
comb$fitted.values = nfl_defense_lm$fitted.values

#### Residuals vs. Fitted Values plot
sz = 20
resid.plot = autoplot(nfl_defense_lm, which = 1, ncol = 1, nrow = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
resid.plot

comb = master[c(3:30, 32:46, 49:52, 55)]

avPlots(nfl_defense_lm)

best_model = glm(comb$combined.epa ~., data=comb)
summary(best_model)

bf.vifs <- vif(nfl_defense_lm)
bf.vifs
mean(bf.vifs)


# Random Forest
rf_mod = randomForest(tot.PA ~., data = comb)
rf_mod
plot(rf_mod)  
varImpPlot(rf_mod, main = "RF Total Points Allowed")  

rf_mod = randomForest(combined.epa ~., data = comb)
rf_mod
plot(rf_mod)  
varImpPlot(rf_mod, main = "RF Combined EPA")  

# library(xgboost)
gbm_mod = gbm(combined.epa ~., data = comb)
summary(gbm_mod, main = "GBM of Combined EPA")

# PDP Plot
plot(gbm_mod,i.var='Sc.')  

# playoffs
playoff_teams = read.csv("playoffteams.csv")
Playoffs_master = merge(master, playoff_teams)
write.csv(Playoffs_master, "Playoffs_master.csv", row.names = FALSE)













