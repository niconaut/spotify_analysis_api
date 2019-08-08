
#' Gets the artist genre from the Spotify API
#' 
#' @param artist_id This is the id of any track on spotify, usually the numbers at the end of the URL (typically after 32 characters)
#' 
#' @param access_token The access token obtained from Spotify using the client and secret IDs
#' 
#' @return The unique artist genre
#' 
#' @example get_artist_genre("246dkjvS1zLTtiykXe5h60")
#'     master_df$artist_id[1] == "246dkjvS1zLTtiykXe5h60"
#' 
#' @export


get_artist_genre <- function(artist_id, access_token = get_spotify_access_token()) {
  
  artist_url <- paste('https://api.spotify.com/v1/artists/',artist_id,sep = '')
  art_gen <- GET(artist_url,query = list(access_token = access_token)) %>% content
  art_gen <- art_gen %>% .$genres
  art_gen <- unlist(unlist(art_gen))
  
  return(art_gen)
  
}

no_genre <- NULL

for (i in 1:nrow(unique_artist)) {
  art_temp <- get_artist_genre(unique_artist$artist_id[i])
  if (is.null(art_temp)) {
    no_genre <- c(no_genre, unique_artist$artist_id[i])
  }
}

unique_genre <- unique_artist[!(unique_artist$artist_id %in% no_genre),]

#' Make columns for each individual genre to store all genres

genres <- unique_genre %>%
  mutate(g1 = "", g2 = "", g3 = "", g4 = "", g5 = "", g6 = "", g7 = "", g8 = "",
         g9 = "", g10 = "", g11 = "", g12 = "", g13 = "", g14 = "", g15 = "", g16 = "",
         g17 = "", g18 = "", g19 = "", g20 = "")

for (i in 1:nrow(unique_genre)) {
  
  artist_temp <- get_artist_genre(unique_artist_genre$artist_id[i])
  length(artist_temp) <- 20
  genres[,c('g1','g2','g3','g4','g5','g6','g7','g8','g9','g10',
            'g11','g12','g13','g14','g15','g16','g17','g18',
            'g19','g20')][i,] <- artist_temp
  
}


# make thing that pulls out top 5 genres!

write.csv(genres, file = 'artist_genres.csv')

#' Merge with master
#' 
merge_master <- data.table(master_df, key = "artist_id")
merge_genres <- data.table(genres, key = "artist_id")
master_df <- merge(merge_master, merge_genres, by = "artist_id", all = TRUE)

write.csv(genres, file = 'master_df_genres.csv')

