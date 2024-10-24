read_swi_analysis <- function(folder_path, nw_params, study_params) {
  
  files <- list.files(path = folder_path, full.names = FALSE)
  n <- length(files) * 10
  
  # initialize vectors
  param <- integer(n)
  subj <- integer(n)
  study <- integer(n)
  cue_set <- integer(n)
  nw_param_p <- integer(n)
  nw_param_r <- integer(n)
  study_param_breadth <- character(n)
  study_param_cue_set_size <- integer(n)
  study_param_cue_repetition <- integer(n)
  indy.swi <- numeric(n)
  fa.swi <- numeric(n)
  rj.swi <- numeric(n)
  
  # provide feedback to the user
  pb <- txtProgressBar(min = 0, max = length(files), initial = 0, style = 3) 
  
  # read files
  for (m_f in 1:length(files)) {
    
    meta_file <- readRDS(paste0(folder_path, files[m_f]))
    
    probe <- meta_file[[1]] # get first cue set data to determine study
    
    m_f_study <- which(study_params$size == probe$parameters$size & 
                         study_params$responses == probe$parameters$responses &
                         study_params$breadth == probe$parameters$breadth)
    
    # add data per 
    for (f in 1:length(meta_file)) {
      
      file <- meta_file[[f]]
      
      i <- (m_f - 1) * 10 + f
      
      # add data to the vectors
      param[i] <- file$parameters$indy_param
      subj[i] <- file$parameters$indy_subj
      study[i] <- m_f_study
      cue_set[i] <- f
      nw_param_p[i] <- nw_params$ps[file$parameters$indy_param]
      nw_param_r[i] <- nw_params$rs[file$parameters$indy_param]
      study_param_breadth[i] <- file$parameters$breadth
      study_param_cue_set_size[i] <- file$parameters$size
      study_param_cue_repetition[i] <- file$parameters$responses
      
      indy.swi[i] <- file$indy_swi$index
      fa.swi[i] <- file$fa_swi$index
      rj.swi[i] <- file$rj_swi$index
      
    }
    
    # update progress bar after each file
    setTxtProgressBar(pb, m_f)
    
  }
  
  # combine data into tibble
  output <- tibble::tibble(param, 
                           subj, 
                           study, 
                           cue_set, 
                           nw_param_p, 
                           nw_param_r,
                           study_param_breadth, 
                           study_param_cue_set_size,
                           study_param_cue_repetition,
                           indy.swi,
                           fa.swi,
                           rj.swi)
  
  close(pb)
  
  output
  
}