
#' Prepping the URL names to pull the dataframe from Spotify's site

dates_2018 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")

url_static <- "https://spotifycharts.com/regional/global/daily/"

all_url <- paste(url_static,dates_2018[1:length(dates_2018)],"/download",sep="")

#' Cleaning up the headers and first lines of the data frames
#' 
#' @param df Any data frame where the first row is the desired header
#'  
#' @return The same dataframe with the first row now being the column names
#' 
#' @export

make_header <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

#' Pulling and merging the daily top 200 lists from Spotify's site

master_df <- NULL

for (i in 1:length(all_url)) {
  
  df <- read.csv(url(all_url[i]))
  df <- make_header(df)
  df$date <- dates_2018[i]
  master_df <- rbind(master_df,df)
  
}

#' Cleaning up the newly merged data frame

row.names(master_df) <- NULL

master_df$URL <- as.character(master_df$URL)

master_df$track_id <- substring(master_df$URL,32)

master_df$track_name <- as.character(master_df$track_name)

master_df$artist <- as.character(master_df$artist)

master_df$streams <- as.integer(master_df$streams)

colnames(master_df) <- tolower(colnames(master_df))

colnames(master_df)[colnames(master_df) == "track name"] <- "track_name"

write.csv(master_df, file = 'master_df_base.csv')


#' @examples 
#' 
#' Some examples of interesting data

#' How many days did 'Drake' appear on top 200
length(unique(subset(master_df, master_df$artist == 'Drake')$date))

#' What days did 'Queen' appear (checkout the dates when the movie was announced and released!)
subset(master_df, master_df$artist == 'Queen')

#' Specific days popularity (Christmas)

artist_by_day <- master_df %>%
  select(artist, streams, date) %>%
  group_by(artist, date) %>%
  summarise(total_daily_streams = sum(streams)) %>%
  arrange(date,desc(total_daily_streams))

#' Christmas day
subset(artist_by_day, artist_by_day$date == '2018-12-25')
