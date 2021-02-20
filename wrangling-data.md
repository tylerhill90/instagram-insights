---
title: "Wrangling Data"
author: "Tyler Hill"
date: "2/12/2021"
output:
  html_document:
    keep_md: true
---



***


```r
# Load all necessary libraries
library(tidyverse)
library(anytime)
```

# The Data

For this project I will using data from my comments, messages, posts, and following/followers. For each of these types I will build tibbles that store the specific data I will need for my analysis. The data I recieved from Instagram is in the form of JSON files and will need to be parsed and cleaned.

## Comment Data

Extract the comment data from "./Data/part_1/comments.json" and create a a tibble from it called df_comments.


```r
# File path
comment_f <- file.path(".", "Data", "part_1", "comments.json")

# Unpack JSON file
df_comments <- jsonlite::read_json(comment_f)

# Extract the data
df_comments <-  sapply(df_comments, `[`, 1:304)
t_stamp <- unlist(sapply(df_comments, `[`, 1))
comm <- unlist(sapply(df_comments, `[`, 2))
reciever <- unlist(sapply(df_comments, `[`, 3))

# Create a tibble of the data
df_comments <- tibble(t_stamp=t_stamp, comment=comm, reciever=reciever)
head(df_comments)
```

```
## # A tibble: 6 x 3
##   t_stamp            comment                                         reciever   
##   <chr>              <chr>                                           <chr>      
## 1 2021-02-02T18:26:… So sorry to hear of her passing. Family is so … rufiomight…
## 2 2021-01-24T02:29:… @samanderbw ahhh, but beautiful chose 😜        monkeyman_…
## 3 2021-01-24T02:12:… Missing these vibes!                            samanderbw 
## 4 2021-01-13T17:12:… @hanoiscrops is this you?                       jerryofthe…
## 5 2021-01-13T17:07:… Love the light in this!                         drupaints  
## 6 2020-11-21T19:09:… @rufiomightfall That sounds like a great bday … monkeyman_…
```

Convert the t_stamp to a better date-time format with the anytime package.


```r
# Convert t_stamp to date-time
df_comments <- df_comments %>%
  mutate(date = anytime(t_stamp)) %>%
  select(!t_stamp)

head(df_comments)
```

```
## # A tibble: 6 x 3
##   comment                                        reciever    date               
##   <chr>                                          <chr>       <dttm>             
## 1 So sorry to hear of her passing. Family is so… rufiomight… 2021-02-02 18:26:36
## 2 @samanderbw ahhh, but beautiful chose 😜       monkeyman_… 2021-01-24 02:29:54
## 3 Missing these vibes!                           samanderbw  2021-01-24 02:12:36
## 4 @hanoiscrops is this you?                      jerryofthe… 2021-01-13 17:12:26
## 5 Love the light in this!                        drupaints   2021-01-13 17:07:53
## 6 @rufiomightfall That sounds like a great bday… monkeyman_… 2020-11-21 19:09:56
```


```r
# Clean workspace
rm(comment_f, t_stamp, comm, reciever)
```

## Connections Data

Extract the connections data from "./Data/part_1/connections.json".


```r
# File path
connections_f <- file.path(".", "Data", "part_1", "connections.json")

# Unpack JSON file
df_connections <- jsonlite::read_json(connections_f)
```

Create a tibble storing all the followers data (who follows me) called df_followers.


```r
# Parse the followers data
df_followers <- df_connections[["followers"]]
df_followers <- sapply(df_followers, `[`, 1)
df_followers <- tibble(
  name = names(df_followers),
  t_stamp = as.character(df_followers)
)

# Convert t_stamp to date-time
df_followers <- df_followers %>%
  mutate(date = anytime(t_stamp)) %>%
  select(!t_stamp)

head(df_followers)
```

```
## # A tibble: 6 x 2
##   name             date               
##   <chr>            <dttm>             
## 1 bigfoot.hikers   2021-02-04 17:26:13
## 2 jpcontrerasc     2021-02-04 17:26:13
## 3 ok.keller        2021-02-03 16:10:28
## 4 kaijacobson      2021-01-31 21:42:51
## 5 blake_ontherocks 2021-01-30 19:02:55
## 6 svell            2021-01-19 16:20:51
```

Create a tibble storing all the following data (who I follow) called df_following.


```r
# Parse the folllowing data
df_following <- df_connections[["following"]]
df_following <- sapply(df_following, `[`, 1)
df_following <- tibble(
  name = names(df_following),
  t_stamp = as.character(df_following)
)

# Convert t_stamp to date-time
df_following <- df_following %>%
  mutate(date = anytime(t_stamp)) %>%
  select(!t_stamp)

head(df_following)
```

```
## # A tibble: 6 x 2
##   name             date               
##   <chr>            <dttm>             
## 1 mac_an_chee      2021-02-03 16:09:35
## 2 ok.keller        2021-02-03 04:24:21
## 3 kaijacobson      2021-01-31 21:42:52
## 4 kalierose327     2021-01-30 19:03:15
## 5 blake_ontherocks 2021-01-30 18:11:13
## 6 miyatsudome      2021-01-24 04:48:44
```


```r
# Clean up workspace
rm(df_connections, connections_f)
```

## Messages Data

The messages data are stored in sub-directories named by the senders name. To extract the data I will store a list of these directory names, iterate through them, and extract their data into a tibble called df_messages.


```r
# File path
messages_dir <- file.path(".", "Data", "part_1", "messages", "inbox")

# List dir names in inbox dir
inbox_names <- list.dirs(messages_dir, full.names=F, recursive=F)

# Store the messages in each subdir in a tibble
df_messages <- tibble(
  sender = character(),
  message = character(),
  t_stamp = numeric()
)
for (name in inbox_names) {
  file_path <- file.path(".", "Data", "part_1", "messages", "inbox", name, "message_1.json")
  df_name <- jsonlite::read_json(file_path)
  df_name <- df_name[["messages"]]
  for (x in 1:length(df_name)) {
    if (!is.null(df_name[[x]][["content"]])) {
      t_stamp <- df_name[[x]][["timestamp_ms"]]
    df_messages <- df_messages %>% add_row(
      sender = gsub("[^[:ascii:]]", "", df_name[[x]][["sender_name"]], perl=T), # Remove non-ascii characters
      message = df_name[[x]][["content"]],
      t_stamp = df_name[[x]][["timestamp_ms"]]
    )
    }
  }
}

# Convert t_stamp to date-time
df_messages <- df_messages %>%
  mutate(date = anytime(t_stamp / 1000)) %>%
  select(!t_stamp)

head(df_messages)
```

```
## # A tibble: 6 x 3
##   sender       message                                       date               
##   <chr>        <chr>                                         <dttm>             
## 1 Tyler Hill   Hello. I saw your post about needing athlete… 2018-05-13 13:16:05
## 2 Tyler Hill   Holy crap!!!! So cool!!!!                     2021-01-23 18:31:37
## 3 Tyler Hill   Cool slab                                     2020-12-03 09:52:17
## 4 Tyler Hill   Chuckanuts?                                   2020-12-03 09:51:58
## 5 Tyler Hill   What'd ya get on?                             2020-10-19 17:52:16
## 6 Adam Starec… Erie days baby! Wa dry!                       2020-10-07 13:19:09
```

```r
#View(df_messages)
```

Afer inspecting the data in the tibble we can see some seemingly erroneous characters in some of the messages. For some reason the emojis are not parsing correctly with jsonlite::read_json() so I will just remove them from the data since we are only concerned about sentiment analysis with words anyways. I will also remove messages that read "Video call started" and "Video call ended" as well as blank messages since they will not help with our analysis.


```r
# Remove non-ascii characters (emojis not handled well when json parsed)
df_messages$message <- gsub("[^[:ascii:]]", "", df_messages$message, perl=T)

# Remove "Video call started/ended" messages
df_messages$message <- gsub("(Video call started|Video call ended)", "", df_messages$message, perl=T)

# Remove blank messages
df_messages <- df_messages %>% filter(message != "")

#head(df_messages)
#View(df_messages)
```


```r
# Clean workspace
rm(x, df_name, file_path, name, inbox_names, messages_dir, t_stamp)
```

## Media Data

Media data includes the data from all of my story, photo, and video posts. This data is located in two files, "./Data/part_2/media.json" and "./Data/part_1/media.json". I will extract their data into a tibble called df_posts.


```r
# File paths
media_f <- file.path(".", "Data", "part_2", "media.json")
media_f2 <- file.path(".", "Data", "part_3", "media.json")

# Unpack he JSON files
df_media <- jsonlite::read_json(media_f)
df_media2 <- jsonlite::read_json(media_f2)

# Stories data
df_stories <- df_media[["stories"]]
caption <- unlist(sapply(df_stories, `[`, 1))
date <- unlist(sapply(df_stories, `[`, 2))
type <- rep("story", length(date))

df_stories <- df_media2[["stories"]]
caption2 <- unlist(sapply(df_stories, `[`, 1))
date2 <- unlist(sapply(df_stories, `[`, 2))
type2 <- rep("story", length(date2))

df_stories <- tibble(date=c(date, date2), caption=c(caption, caption2), type=c(type, type2))

# Photos data
df_photos <- df_media[["photos"]]
caption <- unlist(sapply(df_photos, `[`, 1))
date <- unlist(sapply(df_photos, `[`, 2))
type <- rep("photo", length(date))

df_photos <- df_media2[["photos"]]
caption2 <- unlist(sapply(df_photos, `[`, 1))
date2 <- unlist(sapply(df_photos, `[`, 2))
type2 <- rep("photo", length(date2))

df_photos <- tibble(date=c(date, date2), caption=c(caption, caption2), type=c(type, type2))

# Videos data
df_videos <- df_media[["videos"]]
caption <- unlist(sapply(df_videos, `[`, 1))
date <- unlist(sapply(df_videos, `[`, 2))
type <- rep("video", length(date))

df_videos <- df_media2[["videos"]]
caption2 <- unlist(sapply(df_videos, `[`, 1))
date2 <- unlist(sapply(df_videos, `[`, 2))
type2 <- rep("video", length(date2))

df_videos <- tibble(date=c(date, date2), caption=c(caption, caption2), type=c(type, type2))

# Master posts data tibble by combining stories, photos, and videos data
df_posts <- bind_rows(df_stories, df_photos, df_videos)

# Add a year, month, and day column, then remove dates column
df_posts <- df_posts %>%
  mutate(date = anytime(date))

head(df_posts)
```

```
## # A tibble: 6 x 3
##   date                caption                                              type 
##   <dttm>              <chr>                                                <chr>
## 1 2021-01-31 21:48:09 "Miss you too @annahill_author !\nLooking forward t… story
## 2 2021-01-31 21:45:15 ""                                                   story
## 3 2021-01-31 21:45:12 ""                                                   story
## 4 2021-01-10 04:20:57 "Never gets old!"                                    story
## 5 2020-12-16 00:58:52 "I made dis"                                         story
## 6 2020-12-07 00:48:33 "I feel like a hamster in a wheel"                   story
```

```r
#View(df_posts)
```

From inspecting the data we can see there are some blank and duplicate captions that should be removed from the data for better analysis.


```r
# Clean missing value data
df_posts <- df_posts %>% filter(caption != "")

# Clean duplicate data
df_posts <- df_posts[!duplicated(df_posts$caption), ]
```


```r
# Clean workspace
rm(media_f, media_f2, caption, caption2, date, date2, type, type2, df_photos, df_stories, df_videos, df_media, df_media2)

# Save workspace <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- %>% %>% %>% %>% %>% <- <- <- <- <- <- <- <- 
save.image(file='data.RData')
```

