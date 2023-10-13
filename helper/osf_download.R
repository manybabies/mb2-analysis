gather_osf_data <- function(osf_id, target_dir){

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

