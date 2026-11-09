# data-analysis version

library(data.table)
library(here)
library(lubridate)
library(stringr)

fname_to_label <- function(x = NULL){
  if(is.null(x)) return(NULL)
  if(is.na(x)) return(NULL)
  x_out <- sub("\\.csv$", "", x)
  x_out <- gsub("-", "_", x_out, fixed = TRUE)
  x_out
}


# Trim/squish whitespace and turn tokens into NA. 
# Replaces all internal gaps with single spaces.
clean_text <- function(
    x, 
    tokens_to_na = "", 
    case_sensitive = FALSE
      ) {
  
  x <- stringr::str_squish(as.character(x))
  
  if(!case_sensitive){
    x[!is.na(x) & str_to_lower(x) %in% tokens_to_na] <- NA_character_
  } else {
    x[!is.na(x) & x %in% tokens_to_na] <- NA_character_
  }
  x
}

drop_na <- function(x) {
  x[!is.na(x)]
}


stop_duplicate <- function(x, msg = ""){
  if (any(duplicated(x))) {
    stop(paste0("Duplicated code", msg))
  }
}


push_l <- function(l, value){
  l[length(l)+1] <- value
  l
}

pull_last <- function(l){
  l[length(l)]
}

last_1m <- function(v){
  if(length(v)<2) return(NULL)
  v[length(v)-1]
}

read_extract <- function(l_cfg){
  
  data_dir <- here::here(l_cfg$extract$data_dir)
  
  v_fnames <- list.files(
    data_dir,  pattern = "\\.csv$", full.names = F
    )
  
  stopifnot(length(v_fnames) == l_cfg$extract$num_files)
  
  l_raw <- list()
  
  i = 1
  for(i in seq_along(v_fnames)){
    f <- v_fnames[i]
    
    lab <- fname_to_label(f)
    
    l_raw[[lab]] <- fread(file = here(data_dir, basename(f)))
    
    # check_columns(
    #   d_file_desc$prefix[i],
    #   cfg$data$d_file_desc$cols[i][[1]],
    #   names(l_data_raw[[d_file_desc$name[i]]])
    # )
    
  }
  
  # if(cfg$data$verbose){
  #   
  #   message("Extract data date: ", 
  #           lubridate::as_date(substr(basename(dir_data_raw), 1, 8)), 
  #           " label ",
  #           cfg$report$extract_dttm)
  #   
  #   dat_names <- names(l_data_raw)
  #   for(i in 1:length(l_data_raw)){
  #     message("Dataset: ", dat_names[i], " ncol ", ncol(l_data_raw[[i]]), 
  #             " nrow ", nrow(l_data_raw[[i]]))
  #   }
  #   
  # }
  
  l_raw
  
}
