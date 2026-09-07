library(data.table)
library(here)
library(kableExtra)

read_extract <- function(cfg){
  
  dir_data_raw <- here::here(cfg$data$data_folder)
  d_file_desc <- cfg$data$d_file_desc
  
  v_fnames <- list.files(
    dir_data_raw, 
    pattern = "\\.csv$", full.names = F)
  
  if(length(v_fnames) < nrow(d_file_desc)){
    stop("Incorrect number of files in : ", dir_data_raw)
  }
  
  l_data_raw <- list()
  
  for(i in 1:nrow(d_file_desc)){
    f <- v_fnames[grep(d_file_desc$prefix[i], v_fnames)]
    if (length(f) != 1) {
      stop("Multiple instances of file: ", f[1], " in dir ", dir_data_raw)
    }
    l_data_raw[[d_file_desc$name[i]]] <- 
      fread(file = here(dir_data_raw, basename(f)))
  }
  
  if(cfg$data$verbose){
    
    message("Extract data date: ", 
            lubridate::as_date(substr(basename(dir_data_raw), 1, 8)), 
            " label ",
            cfg$report$extract_dttm)
    
    dat_names <- names(l_data_raw)
    for(i in 1:length(l_data_raw)){
      message("Dataset: ", dat_names[i], " ncol ", ncol(l_data_raw[[i]]), 
              " nrow ", nrow(l_data_raw[[i]]))
    }
    
  }
  
  l_data_raw
  
}