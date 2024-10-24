read_analysis <- function(folder_path, nw_params, study_params) {
  
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
  importance_cor_indy_fa <- numeric(n)
  importance_cor_indy_rj <- numeric(n)
  importance_cor_indy_avg <- numeric(n)
  importance_cor_fa_rj <- numeric(n)
  relatedness_cor_indy_fa <- numeric(n)
  relatedness_cor_indy_rj <- numeric(n)
  relatedness_cor_indy_avg <- numeric(n)
  relatedness_cor_fa_rj <- numeric(n)
  nw_data_indy_n_nodes <- integer(n)
  nw_data_indy_n_edges <- integer(n)
  nw_data_indy_n_giant_comp <- integer(n)
  nw_data_indy_mean_strength <- numeric(n)
  nw_data_indy_cc <- numeric(n)
  nw_data_indy_aspl <- numeric(n)
  nw_data_indy_mod <- numeric(n)
  nw_data_indy_avg_degr <- numeric(n)
  nw_data_fa_n_nodes <- integer(n)
  nw_data_fa_n_edges <- integer(n)
  nw_data_fa_n_giant_comp <- integer(n)
  nw_data_fa_mean_strength <- numeric(n)
  nw_data_fa_cc <- numeric(n)
  nw_data_fa_aspl <- numeric(n)
  nw_data_fa_mod <- numeric(n)
  nw_data_fa_avg_degr <- numeric(n)
  nw_data_rj_n_nodes <- integer(n)
  nw_data_rj_n_edges <- integer(n)
  nw_data_rj_n_giant_comp <- integer(n)
  nw_data_rj_mean_strength <- numeric(n)
  nw_data_rj_cc <- numeric(n)
  nw_data_rj_aspl <- numeric(n)
  nw_data_rj_mod <- numeric(n)
  nw_data_rj_avg_degr <- numeric(n)
  nw_data_avg_n_nodes <- integer(n)
  nw_data_avg_n_edges <- integer(n)
  nw_data_avg_n_giant_comp <- integer(n)
  nw_data_avg_mean_strength <- numeric(n)
  nw_data_avg_cc <- numeric(n)
  nw_data_avg_aspl <- numeric(n)
  nw_data_avg_mod <- numeric(n)
  nw_data_avg_avg_degr <- numeric(n)
  
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
      importance_cor_indy_fa[i] <- file$importance_cor_indy_fa
      importance_cor_indy_rj[i] <- file$importance_cor_indy_rj
      importance_cor_indy_avg[i] <- file$importance_cor_indy_avg
      importance_cor_fa_rj[i] <- file$importance_cor_fa_rj
      relatedness_cor_indy_fa[i] <- file$relatedness_cor_indy_fa
      relatedness_cor_indy_rj[i] <- file$relatedness_cor_indy_rj
      relatedness_cor_indy_avg[i] <- file$relatedness_cor_indy_avg
      relatedness_cor_fa_rj[i] <- file$relatedness_cor_fa_rj
      nw_data_indy_n_nodes[i] <- file$nw_data_indy$n_nodes
      nw_data_indy_n_edges[i] <- file$nw_data_indy$n_edges
      nw_data_indy_n_giant_comp[i] <- file$nw_data_indy$n_giant_comp
      nw_data_indy_mean_strength[i] <- file$nw_data_indy$mean_strength
      nw_data_indy_cc[i] <- file$nw_data_indy$cc
      nw_data_indy_aspl[i] <- file$nw_data_indy$aspl
      nw_data_indy_mod[i] <- max(file$nw_data_indy$mod)
      nw_data_indy_avg_degr[i] <- file$nw_data_indy$avg_degr
      nw_data_fa_n_nodes[i] <- file$nw_data_fa$n_nodes
      nw_data_fa_n_edges[i] <- file$nw_data_fa$n_edges
      nw_data_fa_n_giant_comp[i] <- file$nw_data_fa$n_giant_comp
      nw_data_fa_mean_strength[i] <- file$nw_data_fa$mean_strength
      nw_data_fa_cc[i] <- file$nw_data_fa$cc
      nw_data_fa_aspl[i] <- file$nw_data_fa$aspl
      nw_data_fa_mod[i] <- max(file$nw_data_fa$mod)
      nw_data_fa_avg_degr[i] <- file$nw_data_fa$avg_degr
      nw_data_rj_n_nodes[i] <- file$nw_data_rj$n_nodes
      nw_data_rj_n_edges[i] <- file$nw_data_rj$n_edges
      nw_data_rj_n_giant_comp[i] <- file$nw_data_rj$n_giant_comp
      nw_data_rj_mean_strength[i] <- file$nw_data_rj$mean_strength
      nw_data_rj_cc[i] <- file$nw_data_rj$cc
      nw_data_rj_aspl[i] <- file$nw_data_rj$aspl
      nw_data_rj_mod[i] <- max(file$nw_data_rj$mod)
      nw_data_rj_avg_degr[i] <- file$nw_data_rj$avg_degr
      nw_data_avg_n_nodes[i] <- file$nw_data_avg$n_nodes
      nw_data_avg_n_edges[i] <- file$nw_data_avg$n_edges
      nw_data_avg_n_giant_comp[i] <- file$nw_data_avg$n_giant_comp
      nw_data_avg_mean_strength[i] <- file$nw_data_avg$mean_strength
      nw_data_avg_cc[i] <- file$nw_data_avg$cc
      nw_data_avg_aspl[i] <- file$nw_data_avg$aspl
      nw_data_avg_mod[i] <- max(file$nw_data_avg$mod)
      nw_data_avg_avg_degr[i] <- file$nw_data_avg$avg_degr

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
                           importance_cor_indy_fa,
                           importance_cor_indy_rj,
                           importance_cor_indy_avg,
                           importance_cor_fa_rj,
                           relatedness_cor_indy_fa,
                           relatedness_cor_indy_rj,
                           relatedness_cor_indy_avg,
                           relatedness_cor_fa_rj,
                           nw_data_indy_n_nodes,
                           nw_data_indy_n_edges,
                           nw_data_indy_n_giant_comp,
                           nw_data_indy_mean_strength,
                           nw_data_indy_cc,
                           nw_data_indy_aspl,
                           nw_data_indy_mod,
                           nw_data_indy_avg_degr,
                           nw_data_fa_n_nodes,
                           nw_data_fa_n_edges,
                           nw_data_fa_n_giant_comp,
                           nw_data_fa_mean_strength,
                           nw_data_fa_cc,
                           nw_data_fa_aspl,
                           nw_data_fa_mod,
                           nw_data_fa_avg_degr,
                           nw_data_rj_n_nodes,
                           nw_data_rj_n_edges,
                           nw_data_rj_n_giant_comp,
                           nw_data_rj_mean_strength,
                           nw_data_rj_cc,
                           nw_data_rj_aspl,
                           nw_data_rj_mod,
                           nw_data_rj_avg_degr,
                           nw_data_avg_n_nodes,
                           nw_data_avg_n_edges,
                           nw_data_avg_n_giant_comp,
                           nw_data_avg_mean_strength,
                           nw_data_avg_cc,
                           nw_data_avg_aspl,
                           nw_data_avg_mod,
                           nw_data_avg_avg_degr)
  
  close(pb)
  
  output
  
}
