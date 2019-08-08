
#' Storing data in the database and pulling from it

#' MAKE NAMES SAFE FOR DATABASE
dbSafeNames <- function(names) {
  names <- gsub('[^a-z0-9]+', '_', tolower(names))
  names <- make.names(names, unique=TRUE, allow_=TRUE)
  names <- gsub('.','_',names, fixed=TRUE)
}

#' RENAME MASTER_DF NAMES FOR DATABASE
colnames(master_df) <- dbSafeNames(colnames(master_df))

#' CONNECT TO THE DATABASE
Sys.setenv(SQL_PASSWORD = 'YOUR_PASSWORD')
pg <- dbDriver("PostgreSQL")
driver <- dbDriver("PostgreSQL")
dbconn <- dbConnect(driver, user="YOUR_USERNAME", password=Sys.getenv("SQL_PASSWORD"), host="localhost", port=your-port-num, dbname="YOUR_DB_NAME")

#' CREATE THE DATA FRAME IN THE DATABASE
dbWriteTable(dbconn, 'top_200_of_2018', master_df, row.names=FALSE)

#' PULL DATA FRAME FROM THE DATABASE
master_df_sql <- dbGetQuery(dbconn, "select * from top_200_of_2018")
master_df_sql <- as_data_frame(master_df_sql)
row.names(master_df_sql) <- NULL
master_df_sql <- master_df_sql %>%
  select(position,artist,track_name,streams,date,followers,popularity,url,track_id,artist_id,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms,time_signature,danceability) %>%
  arrange(date, desc(streams))
