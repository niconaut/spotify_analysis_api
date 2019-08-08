
#' Get the artist Spotify popularity 
#' 
#' @param artist_id The Spotify artist's unique id
#' 
#' @param access_token The access token from your Spotify account
#' 
#' @example get_artist_popularity("246dkjvS1zLTtiykXe5h60")
#'     master_df$artist_id[1] == "246dkjvS1zLTtiykXe5h60"
#' 
#' @export

get_artist_popularity <- function(artist_id, access_token = get_spotify_access_token()) {
  
  artist_url <- paste('https://api.spotify.com/v1/artists/',artist_id,sep = '')
  art_pop <- GET(artist_url,query = list(access_token = access_token)) %>% content
  art_pop <- art_pop %>% .$popularity
  return(art_pop)
  
}

#' Suggested method: Use a data frame of unique artists to get their popularity, then merge onto the master data frame

unique_artist <- master_df %>%
  select(artist, track_id) %>%
  distinct(artist, .keep_all = TRUE) %>%
  mutate(artist_id = "")

unique_artist$popularity <- NULL

for (i in 1:nrow(unique_artist)) {
  
  unique_artist$popularity[i] <- get_artist_popularity(unique_artist$artist_id[i])

}

#' Merge the popularity onto the master data frame

master_df <- data.table(master_df, key = "artist_id")
unq_art_merge <- data.table(unique_artist, key = "artist_id")
master_df <- merge(master_df, unq_art_merge, by = "artist_id", all = TRUE)

master_df <- master_df %>%
  arrange(date, desc(streams)) %>%
  dplyr::select()

#' Write the file

write.csv(master_df, file = 'master_df.csv')
