library(pacman)
p_load(tidyverse, skimr, scales, rmarkdown, magrittr, lubridate, janitor, htmltab, ggrepel, viridis,
       ggthemes, knitr, rvest, reactable, RSelenium, stringr.plus, RedditExtractoR, robotstxt, sentimentr)

library(robotstxt)

robot_text <- get_robotstxt("www.fbi.gov")
str(robot_text)

robot_text$permissions

# Check to see if you can scrape (don't have to have paths - just checks to see page by page - can just put domain in there.)
paths_allowed(domain = "https://www.nytimes.com/",paths = c("/section/opinion", "/section/politics"))


# Sys.sleep(runif(1, min = 2, max = 5)) - he suggests building in a random system
# sleep time if you are going to ping a website a bunch of times. 

# Go and collect IP and parse it
read_html("https://api.ipify.org/?format=json")

# Parse into text
read_html("https://api.ipify.org/?format=json") %>% html_text(., trim = TRUE) %>% 
  jsonlite::fromJSON(.)

# Put in fuction:
get_ip <- function(){
  read_html("https://api.ipify.org/?format=json") %>% html_text(., trim = TRUE) %>% 
    jsonlite::fromJSON(.)
}

get_ip()

# Read in page, then get node from inspector in bottom right-hand corner of inspector pane, returns html nodes
read_html("https://www.nytimes.com/section/politics") %>% html_node("p.css-tskdi9.e1hr934v4")

# This is a higher up node
read_html("https://www.nytimes.com/section/politics") %>% html_node("div.css-10wtrbd")

# Grabs only the text within the nodes
read_html("https://www.nytimes.com/section/politics") %>% html_node("div.css-10wtrbd") %>%  html_text2()


# For a table. First read in website:
rugby <- read_html("https://en.wikipedia.org/wiki/Rugby_World_Cup")

# Then, extract the tables from the website (this gets all of them into a list.
# The fill=TRUE turns any missing information into an NA)
rugby_tables <- rugby %>% html_table(., fill = TRUE)  
length(rugby_tables)

# Look around object by object in the list until you find the table you want
# in this case it was the third item on the list, and create a data frame out of it. 
rugby_attendence <- rugby_tables[[3]] %>% as_tibble
rugby_attendence


# Cleaning the table up
rugby_attendence$`Total attend­ance`<- as.numeric(gsub("[,]",replacement = "", rugby_attendence$`Total attend­ance`))
rugby_attendence$`Total attend­ance`
str(rugby_attendence)

rugby_attendence$`Avg attend­ance` <-  as.numeric(gsub("[,]",replacement = "", rugby_attendence$`Avg attend­ance`))
rugby_attendence$`Avg attend­ance`

rugby_attendence$`Attend­ance as % of capacity`<-  as.numeric(sub("%",replacement = "", rugby_attendence$`Attend­ance as % of capacity`))
rugby_attendence$`Attend­ance as % of capacity`

# Put it in a ggplot
rugby_attendence %>% ggplot(aes(x = `Host(s)`, y = `Attend­ance as % of capacity`, fill = `Total attend­ance`)) + 
  geom_bar(stat = "identity")


# Using html_nodes - 2 ways: xpath and css selectors (I should use CSS Selectors)
rugby %>% html_node("div#toc.toc")



# Scrape ecstasy sample page example
ex <- read_html("https://www.drugsdata.org/index.php?sort=DatePublishedU+desc&start=0&a=&search_field=-&m1=-1&m2=-1&datefield=tested&max=500")

ex_tables <- ex %>% html_table(fill = TRUE) %>% .[[2]]

head(ex_tables)

# Then you would clean all of this stuff up






# Interlude...me playing with this stuff on my own for a bit....

# I am going to see if I can get the first few headlines from the NYT Politics page. 

nytp <- read_html("https://www.nytimes.com/section/politics")
nytp_headlines <- nytp %>% html_node("section#collection-highlights-container.css-1vlzjyd.ebvx18p1") 
nytp_head1 <- nytp_headlines %>% html_nodes("a") %>% html_text2()
class(nytp_head1)

# Gets rid of all the blanks.
nytp_head1 <- nytp_head1[nytp_head1 != ""]
nytp_head1






str(salaries)

salaries <- salaries[[1]]


trout <- salaries[1, ]
trout

trimws(trout$Player)

trimws(strsplit(trout$Player, " +")[[1]])

scan(text = trout$Player, what = "")

scan(text = salaries$Player, what = "")

# Try  to figure out why there is only 100 entries being read in - maybe use RSelenium
read_html("https://www.spotrac.com/mlb/rankings/2021/salary/") %>% html_node("table") %>% html_text2()
salaries <- read_html("https://www.spotrac.com/mlb/rankings/2021/salary/") %>% html_table(., trim = TRUE, fill = TRUE)
salaries <- salaries[[1]]

colnames(salaries$`` = "id")
salaries$Player

# Run each player through []
scan(text = salaries$Player[2], what = "")

# example trout
trout <- scan(text = salaries$Player[1], what = "")

player_name <- paste(trout[2], trout[3])

# put into empty list, then unlist it
player_names <- list()
player_names <- player_name
unlist(player_names)

trimws(gsub(pattern = "[\t\n]", replacement = "", x = salaries$Player[1]))


for (i in 1:length(salaries$Player)) {
  player <- unlist(str_split(str_squish(gsub(pattern = "[\t\n]", replacement = "", x = salaries$Player[i])), " "))
player_names <- append(player_names, values = paste(player[2], player[3]))
}
  
player_names
trout

player <- unlist(str_split(str_squish(gsub(pattern = "[\t\n]", replacement = "", x = salaries$Player[3])), " "))
player
length(salaries$Player)




# This is the function to clean up the names
for (i in 1:length(salaries$Player)) {
  player_name <- salaries$Player[i] %>% gsub(pattern = "[\t\n]",replacement = "", x = .) %>% #have to remove the \t\n\r's first or str_squish won't do much
    str_squish(.) %>% # gets rid of all the extra space in between the words
    #Don't need below - easier way - word() - see below
    # strsplit(., " ") %>% #we have too many words inside our string, so we are going to make them their own separate strings to get rid of them
    # unlist(.) #strsplit puts the strings into a list, so we unlist them to make them a vector again.
player_names <- append(player_names, paste(player_name[2], player_name[3])) #just pick the first and second elements of the vector to get the player's name
}

unlist(player_names)
# SO after working on this for like 2 hours, it is just easier to copy and paste the table into Excel and manipulate from there. Just do that. 



salaries$player
#This is the function you want
salaries$Player[1] %>% gsub(pattern = "[\t\n]",replacement = "", x = .) %>% str_squish(.) %>% 
  word(start = 2, end = 3)
#Then you would just append to the list