# keep this around in case osfr gets fixed, as the conflict resolution would be nice to have
gather_osf_data_broken <- function(osf_id, target_dir){

  files <- osfr::osf_retrieve_node(osf_id) %>% 
    osfr::osf_ls_files(n_max = 1000)
  
  files |>
    mutate(idx = 1:n()) %>%
    base::split(.$idx) |>
    map(function(f) {
      print(f$name[1])
      osfr::osf_download(f, 
                         path = here(target_dir), 
                         conflicts = "skip", 
                         progress = TRUE)
    }) 
}

library(httr)
library(glue)
library(utils)

gather_osf_data <- function(osf_id, target_dir){
  if(dir.exists(target_dir) && length(list.files(target_dir)) != 0){
    return(paste0("Data already exists in dir ", target_dir))
  }
  
  zip_path <- paste0(target_dir, ".zip")
  
  glue::glue("https://files.osf.io/v1/resources/{osf_id}/providers/osfstorage/?zip=") %>%
    curl::curl_download(zip_path, quiet = FALSE)
  
  unlink(target_dir, recursive=TRUE)
  utils::unzip(
    zip_path,
    overwrite = TRUE,
    exdir = target_dir
  )
  
  file.remove(zip_path)
}