require(tidyverse)
require(httr)
require(jsonlite)


#---------------------------- Load game & movie metadata ------------------------------

games <- read_csv("./data/movie_themed_slot_games.csv",
                  col_types = cols(game_release_date = col_date(format = "%Y-%m-%d")))


# --------------------------- Collect data from TMDB ----------------------------------

# Please put your own The Movie Database API here:
api_key <- '<your API key here>'

# Base TMDB API URL:
base_url <- "https://api.themoviedb.org/3/movie/"


# Function to collect general movie info:
get_tmdb_movie <- function(df, api_key) {
  
  request_url <- paste0(base_url, df$tmdb_movie_id, 
                        '?api_key=', api_key, '&language=en-US')
  
  tryCatch(
    
    expr = {
      
      r <- GET(request_url)
      
      if (r$status_code != 200) {stop("Status: ", r$status_code)}
      
      result <- content(r, "text") %>% fromJSON(.)
      result_tbl <- tibble(budget = result$budget,
                           overview = result$overview,
                           runtime = result$runtime,
                           genre = list(result$genres$name), 
                           production_companies = list(result$production_companies$name))
      
      if (nrow(result_tbl) == 0L) {
        Sys.sleep(1)
        return(tibble(budget = NA,
                      overview = NA,
                      runtime = NA,
                      genre = NA, 
                      production_companies = NA))
      }
      
      Sys.sleep(1)
      
      return(result_tbl)
      
    },
    
    error = function(e) {
      message("Got an error: ", e, "\n", "Movie ID: ", df$tmdb_movie_id)
    }
    
  )
  
}

tmdb_overview <- games %>%
  group_by(movie_id) %>%
  slice(1) %>%
  do(get_tmdb_movie(., api_key))


# Utility function to extract genre tags and create a wide dataframe:
genres_long <- function(df) {
  genre <- df$genre %>% 
    unlist() %>% 
    tolower() %>% 
    gsub(" ", "_", .)
  result <- tibble::enframe(name = NULL, genre)
  names(result) <- "genre"
  return(result)
}

genres <- tmdb_overview %>%
  select(movie_id, genre) %>%
  group_by(movie_id) %>%
  do(genres_long(.)) %>%
  mutate(presence = 1) %>%
  tidyr::pivot_wider(id_cols = movie_id, 
                     names_from = genre, 
                     values_from = presence,
                     values_fill = list(presence = 0)) %>%
  tidyr::unnest(., cols = adventure:western)


# Function to collect movie credits:
get_tmdb_credits <- function(df, api_key) {
  
  request_url <- paste0(base_url, df$tmdb_movie_id, 
                        '/credits?api_key=', api_key)
  
  tryCatch(
    
    expr = {
      
      r <- GET(request_url)
      
      if (r$status_code != 200) {stop("Status: ", r$status_code)}
      
      result <- content(r, "text") %>% fromJSON()
      result$cast <- result$cast %>% filter(order <= 4)
      result$crew <- result$crew %>% filter(department == "Directing",
                                            job == "Director")
      
      result <- tibble(movie_id = result$id,
                       credits = c("cast", "crew"),
                       data = list(
                         result$cast,
                         result$crew)
      )
      
      Sys.sleep(1)
      
      return(result)
      
    },
    
    error = function(e) {
      message("Got an error: ", e, "\n", "Movie ID: ", df$tmdb_movie_id)
    }
    
  )
  
}

tmdb_credits <- games %>%
  group_by(movie_id) %>%
  slice(1) %>%
  do(get_tmdb_credits(., api_key))


# Function to collect MPAA ratings:
get_tmdb_certificates <- function(df, api_key) {
  
  request_url <- paste0(base_url, df$tmdb_movie_id,
                        '/release_dates?api_key=', api_key)
  
  tryCatch(
    
    expr = {
      
      r <- GET(request_url)
      
      if (r$status_code != 200) {stop("Status: ", r$status_code)}
      
      result <- content(r, "parsed")
      usa_index <- sapply(result$results, function(l){l[[1]] == "US"})
      
      if (!any(usa_index)) {
        Sys.sleep(1)
        return(tibble(tmdb_movie_id = result$id, mpaa = NA))
      }
      
      if (length(result$results[usa_index][[1]]) == 1) {
        
        mpaa <- result$results[usa_index][[1]]$release_dates[[1]]$certification
        
      } else {
        
        cert_index <- sapply(result$results[usa_index][[1]]$release_dates,
                             function(l){l$certification != ""})
        
        if (!any(cert_index)) {
          Sys.sleep(1)
          return(tibble(tmdb_movie_id = result$id, mpaa = NA))
        }
        
        mpaa <- result$results[usa_index][[1]]$release_dates[cert_index][[1]]$certification
        
      }
      
      result <- tibble(tmdb_movie_id = result$id, mpaa = mpaa)
      
      Sys.sleep(1)
      
      return(result)
      
    },
    
    error = function(e) {
      message("Got an error: ", e, "\n", "Movie ID: ", df$tmdb_movie_id)
    }
    
  )
  
}

tmdb_certificates <- games %>%
  group_by(movie_id) %>%
  slice(1) %>%
  do(get_tmdb_certificates(., api_key))


# Save data objects for further analysis:
save(games, 
     tmdb_overview, 
     tmdb_certificates, 
     tmdb_credits,
     genres,
     file = "./workspaces/game_and_movie_data.RData")