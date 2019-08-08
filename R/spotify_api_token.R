
#' Getting the access token using Spotify 'Client ID' and 'Client Secret ID'
#' Code from https://github.com/charlie86/spotifyr/blob/master/R/artists.R

#' Use 'Sys.setenv' to store the IDs, then 'Sys.getenv' to retrieve them

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR CLIENT ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR SECRET ID')

#' Creating the access token
#' 
#' @param client_id The Spotify client ID
#' 
#' @param client_secret The Spotify client secret ID
#' 
#' @return The access token to pull from Spotify's API
#' 
#' @example access_token <- get_spotify_access_token()
#' 
#' @export

get_spotify_access_token <- function(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET')) {
  
  post <- POST('https://accounts.spotify.com/api/token',
               accept_json(), authenticate(client_id, client_secret),
               body = list(grant_type = 'client_credentials'),
               encode = 'form', httr::config(http_version = 2)) %>% content
  
  access_token <- post$access_token
  
  return(access_token)
}
