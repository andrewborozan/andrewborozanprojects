library(pacman)
p_load(tidyverse, skimr, scales, rmarkdown, magrittr, lubridate, janitor, htmltab, ggrepel, 
       ggthemes, showtext, knitr, rvest, reactable, stringr.plus)
link <- "https://www.imdb.com/search/name/?birth_monthday=02-19&ref_=nv_cel_brn"
bdays_page <- read_html(link)

names <- bdays_page %>% html_elements(css = ".lister-item-header a") %>% html_text2()
known_for <- bdays_page %>% html_elements(css = ".text-small") %>% html_text2()
blurb <- bdays_page %>% html_nodes(css = ".text-small+ p") %>% html_text2()
blurb
set <- data_frame(names, known_for, blurb)

library("stringr.plus")
install.packages("stringr.plus")

install.packages("remotes")
remotes::install_github("johncassil/stringr.plus")


names






pantries <- read_html("https://www.foodpantries.org/ci/ca-san_francisco")
pantries_page_text <- pantries %>% html_elements(".span8") %>% 
  html_text2()
pantries_page_text
pantry_address <- pantries_page_text %>% 
  str_extract_between('\"streetAddress\":\"', '\",\r \"addressLocality\":\"', which_pattern1 = "first", which_pattern2 = "first")
pantry_address

pantry_zip <- pantries_page_text %>% 
  str_extract_between(pattern1 = '\r \"name\": \"', pattern2 = '\"\r ,\"image\": \"', which_pattern1 = "first",
                      which_pattern2 = "first")
pantry_zip


pantry_name <- pantries %>%
  html_elements("h1") %>% 
  html_text2() %>%
  as_tibble() %>%
  filter(value != "Food Pantries") %>%
  mutate(value = str_remove(value, " Details Page")) %>%
  pull(value)




reddit_page <- read_html("https://www.reddit.com/r/politics/comments/sx1jt2/california_lawmakers_introduce_bill_to_tax/")

(title <- reddit_page %>% html_node("#t3_sx1jt2 ._eYtD2XCVieq6emjKBH3m") %>% 
  html_text2())

post <- reddit_page %>% html_elements() %>% html_text2()
length(post)


page_text <- reddit_page %>% html_text2()
page_text %>% str_count('\"t\":\"')


reddit_link <- "https://www.reddit.com/r/politics/comments/sx1jt2/california_lawmakers_introduce_bill_to_tax/"
reddit_html <- read_html(reddit_link)

posts <- reddit_html %>% html_nodes("p._1qeIAgB0cPwnLhDF9XSiJM") %>% html_text()
posts



super_node <- reddit_html %>% html_node("div._2M2wOqmeoPVvcSsJ6Po9-V")
post_node <- "div._3cjCphgls6DH-irkVaA0GM "

posts <- html_nodes(super_node, post_node) %>% html_text()
posts

install.packages("RSelenium")
library(RSelenium)

RSelenium::rsDriver()
install.packages("netstat")
library(netstat)


#start the server
rs_object <- rsDriver(browser = "chrome", chromever = "98.0.4758.102",
                      verbose = FALSE, 
                      port = free_port())
remDR <- rs_object$client

#open a browser
remDR$open()

#load page
remDR$navigate("https://www.imdb.com/search/name/?birth_monthday=02-20&start=151&ref_=rlm")



#find element
star_name <- remDR$findElements(using = "class name", "lister-item-header")
length(star_name)


posts_lists <- lapply(posts, function(x) x$getElementText()) %>% unlist()
posts_lists <- trimws(posts_lists)
posts_lists
class(posts_lists)
str(posts_lists)
work <- data.frame(seq(1, length(posts_lists)), posts_lists)


blurb <- remDR$findElements(using = "class", "lister-item-content")
length(blurb)

blurb
unlist(blurb) 

name_star <- unlist(lapply(blurb, function(x) x$getElementText()))
name_star              


imdb <- read_html("https://www.imdb.com/search/name/?birth_monthday=02-20&start=151&ref_=rlm")
blurbs <- imdb %>% html_nodes(".lister-item-content") %>% html_text2()
blurbs


just_name <- lapply(blurbs, function(x) {
  x <-substring(x, 6)
  str_extract_before(x, "\n", which = "first")
})
just_name <- unlist(just_name)
blurbs

known_for <- str_extract_between(blurbs, pattern1 = "\n", 
                                 pattern2 = "\n", 
                                 which_pattern1 = "first",
                                 which_pattern2 = "last") 
known_for
known_for <- str_replace_all(known_for, "[\n]", "")

birthdays <- data.frame(just_name, known_for)
blurbs[48]
view(blurbs)




reddit_story <- read_html("https://www.reddit.com/r/politics/comments/swdwy7/biden_promotes_1billion_great_lakes_cleanup_push/")
comments <- reddit_story %>% html_elements("p._1qeIAgB0cPwnLhDF9XSiJM") %>% html_text2()
length(comments)
comments






























# The first thing we are going to do is read in data from Reddit threads using Selenium. 
library(RSelenium)

# The netstat package gives us a port to use when opening a browser.
library(netstat)

# This fires up a browser we can control through R. There are two objects in rs_object, so we
# need to specify we will be working with the client. (Can't have your VPN on for this to work.)
rs_object <- rsDriver(browser = "chrome", chromever = "98.0.4758.102",
                      verbose = FALSE, 
                      port = free_port())
remDr <- rs_object$client
remDr

# with selenium pipes
remmDr <- remoteDr(port = 14415L, browserName = "chrome")
# We then navigate to the page we want to scrape from.
remDR$navigate("https://www.reddit.com/r/politics/comments/sxiox4/gop_senate_candidate_jd_vance_said_he_doesnt/")

# We find the element using the selector tool in a normal browser. Use "class" and then put the selector tool tag.
comments1 <- remDR$findElements(using = "class name", "_1qeIAgB0cPwnLhDF9XSiJM")
length(comments1)
# "p._1qe..." doesn't work. Moving up one class doesn't really work either. The most comments retrieved were from
# the tag in the selector tool and class name. Let's try by tag name, using "p"
comments2 <- remDR$findElements(using = "tag name", "p")
length(comments2)
# Only slightly more rendered with this method. 149 as opposed to the above which spit out 147. 
# Let's name them different things and compare when running them through the text grabber. 
# And let's try CSS Selector while we are at it. 
comments3 <- remDR$findElements(using = "css selector", "p._1qeIAgB0cPwnLhDF9XSiJM")
length(comments3)
# Same as "class name". Let me try going up a class where there is a "data-testid":
comments4 <- remDR$findElements(using = "id", "comment")
length(comments4)
# That didn't work at all. No results. There is a json text class right before the class I used above. 
# We will try that:
comments5 <- remDR$findElements(using = "class name", "RichTextJSON-root")
length(comments5)
# Got 115 out of this one. 

# How about this class:
comments6 <- remDR$findElements(using = "class name", "_292iotee39Lmt0MkQZ2hPV")
length(comments6)
# Same, 115. 



# One last test with selenium. XPath:
comments7 <- remDR$findElements(using = "xpath", '//*[@id="t1_hxszmnm"]/div[2]/div[2]/div[2]/div/p[2]')
length(comments7)
# Nope. Only 1 item returned. But this does give me another option?
comments8 <- remDR$findElements(using = "id", "t1_hxszmnm")
length(comments8)
# Nope. Only 1 again. So, it looks like our best bet with selenium is going with comments2 "tag name", "p"

# What does rvest return?
reddit_page <- read_html("https://www.reddit.com/r/politics/comments/sxiox4/gop_senate_candidate_jd_vance_said_he_doesnt/")
class(reddit_page)
# First with nodes:
comments_rvest1 <- reddit_page %>% html_nodes("._1qeIAgB0cPwnLhDF9XSiJM") %>% html_text2()
length(comments_rvest1)
# 29, not a lot.

# Try again:
comments_rvest2 <- reddit_page %>% html_nodes("p._1qeIAgB0cPwnLhDF9XSiJM") %>% html_text2()
length(comments_rvest2)
# Same, 29. Elements?

comments_rvest3 <- reddit_page %>% html_elements("p._1qeIAgB0cPwnLhDF9XSiJM") %>% html_text2()
length(comments_rvest3)
# Same, 29. So, it looks like selenium is our winner with 149. But let's see what is in that 149. 

# We need to get this into text. So, two options (try both to see if they work)
# 1) put it into a function with getElementText()
# 2) try html_text2()

# First I will try with just the first three "comments" from Selenium. 
reddit_comments1 <- unlist(lapply(comments1, function(x) x$getElementText()))
reddit_comments1
# This seemed to work just fine. 

reddit_comments2 <- unlist(lapply(comments2, function(x) x$getElementText()))
reddit_comments2
# Exact same as 1 only some extraneous material that doesn't belong anyway. 

# Don't think 3 will be any different from 1 but let's run it anyway:
reddit_comments3 <- unlist(lapply(comments3, function(x) x$getElementText()))
reddit_comments3

# Next, let's try pumping in some selenium lists into html_text2.
reddit_comments1_rvest <- html_text2(comments1)
# Didn't work - it is reading a list, not html. Guessing it is the same for comments2...
reddit_comments2_rvest <- html_text2(comments2)
# Obviously didn't work. Is there a way to just get the html from selenium, then feed that into rvest?

# Let's try to get the html from selenium and then have rvest read it?
reddit_html <- remDR$getPageSource()
class(reddit_html)
reddit_html_text <- unlist(reddit_html)
class(reddit_html_text)

# Ok, so I just got the page source, but it came back as a list. I unlisted it so it became a character stirng.
# I have to turn it back into xml for it to be read by rvest. XML package has this ability?
library(XML)
reddit_html_text <- xml(reddit_html_text)
class(reddit_html_text)
# That gave back two items. Let's pass the first into the object:
reddit_html_text <- reddit_html_text[1]

reddit_html_text
# Looks like that made it an xmlstring type. Let's try reading this html with rvest
reddit_comments_rvest_seleniumhtml <- reddit_html_text %>% html_nodes("_1qeIAgB0cPwnLhDF9XSiJM") %>% 
  html_text2()
#Looks like that didn't work. Maybe if I unlist the reddit_html?
reddit_html <- unlist(reddit_html)
# Now let's try running it through again?
reddit_comments_rvest_seleniumhtml <- reddit_html %>% html_nodes("_1qeIAgB0cPwnLhDF9XSiJM") %>% 
  html_text2()
# No. Something is wrong where the class of the reddit_html is a list. I need it as an xml. Hmmm....let's try:
remmDr %>% go("https://www.reddit.com/r/politics/comments/sxiox4/gop_senate_candidate_jd_vance_said_he_doesnt/")
# Didn't work because you need a different package

install.packages("seleniumPipes")
library(seleniumPipes)

# I spent three hours trying to figure out SeleniumPipes, but there is something wrong where my remote browser doesn't
# maintain it's remote ID after it navigates to a page. I spent a long time looking up how to fix this on the internet and
# and found one comment about it...with no solution posted. So, I am just going to stick with RSelenium, which did 
# seem to do the job of pulling the comments from the Reddit page. 

# After looking at the output for the first method of extracting comments (by class) and the second method (by tag)
# it seems that the first groups them and doesn't have extraneous material that the tag method returns. From now on
# then we will be using just the first set of extracted comments - reddit_comments1 - to perform the analysis. 

# We want to filter out reddit bot comments so they don't skew our results.

# Let's also get the user name and tie it to the comment made. 
# But, I want to do this with rvest because there is a problem - the user name tags extend to material all the way
# down the page, and there are posts that have nothing to do with the original news article. I can do the "supernode"
# technique of grabbing just a subset of the html contents and then run the node scraper. 

movies <- data.frame()

#for (page_result in seq(from = 1, to = 151, by = 50)) {
  
  super_node <- '.uI_hDmU5GSiudtABRz_37' 
  
  # read as vector of all blocks of supernode (imp: use html_nodes function)
  super_node_read <- html_nodes(reddit_page, super_node)
  
  # define each node element that you want
  user_handle <- ''

  
  
  # extract the output for each as cleaned text (imp: use html_node function)
  name <- html_node(super_node_read, name_css) %>%
    html_text() %>% trimws() %>% 
    str_replace_all("[\t\n\r]" , "")
  
  known_for <- html_node(super_node_read, known_for_css) %>%
    html_text() %>% trimws() %>% 
    str_replace_all("[\t\n\r]" , "")
  
  blurb <- html_node(super_node_read, blurb_css) %>%
    html_text() %>% trimws() %>% 
    str_replace_all("[\t\n\r]" , "")
  
  movies = rbind(movies, data.frame(name, known_for, blurb))
}

disclaimers <- c(
  "As a reminder, this subreddit is for civil discussion.",
  "In general, be courteous to others. Debate/discuss/argue the merits of ideas, don't attack people. Personal insults, shill or troll accusations, hate speech, any suggestion or support of harm, violence, or death, and other rule violations can result in a permanent ban." ,
  "If you see comments in violation of our rules, please report them." ,
  "For those who have questions regarding any media outlets being posted on this subreddit, please click here to review our details as to our approved domains list and outlet criteria.",
  "I am a bot, and this action was performed automatically. Please contact the moderators of this subreddit if you have any questions or concerns."
)

reddit_comments1_nd <- reddit_comments1 %>% 
  f



# Going to try a different method here. Going to take just the main section and extract each one of these: #t1_hxtqwhn
# Then, write a for loop that extracts user name and comment.

comment_table <- data.frame()

comment_section <- html_nodes(super_node_read, "_3sf33-9rVAO_v4y0pIW_CH")

class(super_node_read)


class(comment_section)

user_handle <- html_nodes(reddit_page, "class", "_3QEK34iVL1BjyHAVIeVVNQ") %>% 
  html_text2()

length(user_handle)
comment_section
super_node_read


# All of this in rvest didn't work. Couldn't figure it out. 

# Try the idea in RSelenium


rs_object <- rsDriver(browser = "chrome", chromever = "98.0.4758.102",
                      verbose = FALSE, 
                      port = free_port())
remDR <- rs_object$client

# We then navigate to the page we want to scrape from.
remDR$navigate("https://www.reddit.com/r/politics/comments/sxiox4/gop_senate_candidate_jd_vance_said_he_doesnt/")

comment_chunks <- remDR$findElements(using = "class name", "_3tw__eCCe7j-epNCKGXUKk")
class(comment_chunks)
comment_chunks[[2]]


str_extract_between()

class(comments1)










as.character(comment_chunk[[2]])

comment_chunk <- remDR$findElements(using = "class name", "_3tw__eCCe7j-epNCKGXUKk")
length(comment_chunk)

for(i in comment_chunk) {
  
}


hate <- read_html("https://www.reddit.com/r/politics/comments/sxiox4/gop_senate_candidate_jd_vance_said_he_doesnt/")

hate1 <- xmlParse("https://www.reddit.com/r/politics/comments/sxiox4/gop_senate_candidate_jd_vance_said_he_doesnt/")

parsedhate <- htmlParse(hate)
class(parsedhate)

xmlhate <- xmlToList(parsedhate)
xmlhate

xmlhate[['/html/body/div[2]/div/div[2]/div[2]/div/div[3]/div[1]/div[2]/div[6]/div/div/div/div[17]/div/div/div/div[2]/div[2]/div[2]']]


data <- htmlTreeParse("https://www.reddit.com/r/politics/comments/sy41r6/white_house_biden_to_prohibit_trade_investment/")
data %>% xml_find_all('//*[@id="t1_hxxierk"]/div[2]/div[2]/div[2]/div')
hate1

xmlParse(data)
data
unlist(data)
hate <- as.character(data)
str_e

install.packages("RedditExtractoR")
devtools::install_github('ivan-rivera/RedditExtractor')

