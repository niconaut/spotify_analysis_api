
#' Gets the Artist ID from the Spotify API
#' 
#' @param track_id This is the id of any track on spotify, usually the numbers at the end of the URL (typically after 32 characters)
#' 
#' @param access_token The access token obtained from Spotify using the client and secret IDs
#' 
#' @return The unique artist ID
#' 
#' @export

get_artist_id <- function(track_id, access_token = get_spotify_access_token()) {
  
  track_url <- paste('https://api.spotify.com/v1/tracks/',track_id,sep = '')
  art_id <- GET(track_url,query = list(access_token = access_token)) %>% content
  art_id <- art_id %>% .$artists
  art_id <- art_id[[1]]$id
  return(art_id)
  
}

#' Suggested use: Get the list of unique artists to lessen the API calling load

unique_artist <- master_df %>%
  select(artist, track_id) %>%
  distinct(artist, .keep_all = TRUE) %>%
  mutate(artist_id = "")

for (i in 1:nrow(unique_artist)) {
  unique_artist$artist_id[i] <- get_artist_id(unique_artist$track_id[i])
}

#' Get just the list of unique artists and artist_id to merge

unique_artist <- unique_artist %>%
  select(artist,artist_id)

#' Merge the new artist_id data frame with the master data frame

merge_master <- data.table(master_df, key = "artist")
merge_unq_artist <- data.table(unique_artist, key = "artist")
master_df <- merge(merge_master, merge_unq_artist, by = "artist", all = TRUE)

master_df <- master_df %>%
  select(position,artist,track_name,streams,date,url,track_id,artist_id) %>%
  arrange(date, desc(streams))

#' Suggested write file to not keep doing the same API calls 

write.csv(master_df, file = 'master_df_artist_id.csv')

#' @example
#' Total streams by artist (Post Malone)

all_streams <- master_df %>%
  select(artist, streams, artist_id) %>%
  group_by(artist, artist_id) %>%
  summarise(total_streams = sum(as.numeric(streams))) %>%
  arrange(desc(total_streams))

subset(all_streams, all_streams$artist == 'Post Malone')
