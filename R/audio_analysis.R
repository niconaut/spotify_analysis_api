
#' Get all the different audio features Spotify offers
#' 
#' @param track_id The Spotify track ID
#' 
#' @param access_token Your Spotify token based on the client and secret keys
#' 
#' @example get_audio_features("7wGoVu4Dady5GV0Sv4UIsx")
#'     master_df$track_id[1] == "7wGoVu4Dady5GV0Sv4UIsx"
#' 
#' @export

get_audio_features <- function(track_id, access_token = get_spotify_access_token()) {
  
  track_url <- paste('https://api.spotify.com/v1/audio-features/',track_id,sep = '')
  track_feat <- GET(track_url,query = list(access_token = access_token)) %>% content
  return(track_feat)
  
}

#' Suggested method: get a unique list of tracks then do *one* call for all audio features and sort them after the call has been made

unique_tracks <- master_df %>%
  select(track_id) %>%
  distinct(track_id, .keep_all = TRUE) %>%
  mutate(energy = as.integer(""), key = as.integer(""), loudness = as.integer(""), mode = as.integer(""), speechiness = as.integer(""), acousticness = as.integer(""), instrumentalness = as.integer(""), liveness = as.integer(""), valence = as.integer(""), tempo = as.integer(""), duration_ms = as.integer(""), time_signature = as.integer(""), danceability = as.integer(""))

for (i in 1:nrow(unique_tracks)) {
  
  track_features <- get_audio_features(unique_tracks$track_id[i])
  track_features <- track_features[c('energy', 'key', 'loudness',
                                     'mode','speechiness','acousticness','instrumentalness',
                                     'liveness', 'valence', 'tempo', 'duration_ms',
                                     'time_signature', 'danceability')]
  unique_tracks[,c('energy','key','loudness','mode','speechiness',
                   'acousticness','instrumentalness','liveness','valence',
                   'tempo','duration_ms','time_signature','danceability')][i,] <- track_features
  
}

#' Write the file

write.csv(unique_tracks, file = 'audio_analysis.csv')

#' Merging the audio features onto the master data frame

merge_master <- data.table(master_df, key = "track_id")
merge_track <- data.table(unique_tracks, key = "track_id")
master_df <- merge(merge_master, merge_track, by = "track_id", all = TRUE)

master_df <- master_df %>%
  arrange(date, desc(streams))

#' Write the file

write.csv(master_df, file = 'master_df_audio_anaysis.csv')
