source("code/process/analysis_inputs/index_functions.R")
# Compute a weighted Pearson correlation after dropping incomplete observations.
weighted_cor <- function(x, y, w) {
  complete <- complete.cases(x, y, w) & w > 0
  x <- x[complete]
  y <- y[complete]
  w <- w[complete]

  # Return missing when the data cannot identify a correlation.
  if (length(x) < 2 || sum(w) == 0 || stats::weighted.mean((x - stats::weighted.mean(x, w))^2, w) == 0 ||
      stats::weighted.mean((y - stats::weighted.mean(y, w))^2, w) == 0) {
    return(NA_real_)
  }

  # Use weighted covariance divided by weighted standard deviations.
  x_mean <- stats::weighted.mean(x, w)
  y_mean <- stats::weighted.mean(y, w)
  cov_xy <- stats::weighted.mean((x - x_mean) * (y - y_mean), w)
  sd_x <- sqrt(stats::weighted.mean((x - x_mean)^2, w))
  sd_y <- sqrt(stats::weighted.mean((y - y_mean)^2, w))
  cov_xy / (sd_x * sd_y)
}

# Pick most mentioned var
create_varlist_mostmention=function(raw_data_manifesto,cw_manfiesto_labels_forManifesto){
  raw_data_manifesto_mostmention=raw_data_manifesto%>%
    select(-peruncod)%>%
    rename(weight=pervote)%>%
    mutate(across(starts_with("per"),~.x*weight))%>%
    group_by(manifesto_country_name)%>%
    summarise(across(starts_with("per"),~mean(.x,na.rm=T)))%>%
    ungroup()%>%
    summarise(across(starts_with("per"),~mean(.x,na.rm=T)))
  raw_data_manifesto_mostmention_long = raw_data_manifesto_mostmention %>%
    pivot_longer(cols = everything(),names_to = "variable",values_to = "value")
  temp=raw_data_manifesto_mostmention_long%>%
    arrange(desc(value)) %>%
    top_n(5)
  temp=left_join(temp,cw_manfiesto_labels_forManifesto)
  list_mention=as.character(temp$title)
  return(list_mention)
}

create_varlist_highvar=function(df,wave_base){
  df_parfam_mean=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    group_by(BB_Country,BBF_parfam,variable)%>%
    summarise(value=weighted.mean(value,pervote,na.rm=T))%>%
    group_by(BBF_parfam,variable)%>%
    summarise(value=mean(value,na.rm=T))%>%
    ungroup()
  df_parfam_variance=df_parfam_mean%>%
    group_by(variable)%>%
    summarise(variance=var(value,na.rm=T))
  temp=df_parfam_variance%>%
    arrange(desc(variance)) %>%
    top_n(8)
  list_highbtwgrvar=as.array(temp[[1]])
  return(list_highbtwgrvar)
}

# largest variance between groups / total variance of weights
create_varlist_highvar_sd=function(df,wave_base){
  # Compute Mean by Parfam
  df_parfam_mean=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    group_by(BB_Country,BBF_parfam,variable)%>%
    summarise(value=weighted.mean(value,pervote,na.rm=T))%>%
    group_by(BBF_parfam,variable)%>%
    summarise(value=mean(value,na.rm=T))%>%
    ungroup()
  # Compute across-group variance
  df_parfam_variance=df_parfam_mean%>%
    group_by(variable)%>%
    summarise(gr_variance=var(value,na.rm=T))
  # Compute total variance
  df_tot_variance=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    group_by(variable)%>%
    summarise(tot_variance=var(value,na.rm=T))%>%
    ungroup()
  # Compute across-group variance/total variance
  temp=left_join(df_parfam_variance,df_tot_variance)%>%
    mutate(variance=gr_variance/tot_variance)%>%
    arrange(desc(variance)) %>%
    top_n(8)
  list_highbtwgrvar_sd=as.array(temp[[1]])
  return(list_highbtwgrvar_sd)
}

# largest variance between groups / total variance of weights, weighted by # of voters for each parfam
create_varlist_highvar_weighted_sd=function(df,wave_base){
  # Compute Mean by Parfam
  df_parfam_mean=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    group_by(BB_Country,BBF_parfam,variable)%>%
    summarise(value=weighted.mean(value,pervote,na.rm=T))%>%
    group_by(BBF_parfam,variable)%>%
    summarise(value=mean(value,na.rm=T))%>%
    ungroup()
  # # of voters by parfam in this data sets
  df_parfam_weight=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    select(BB_Country,BBF_parfam,pervote)%>%
    group_by(BB_Country,BBF_parfam)%>%
    summarise(pervote=mean(pervote,na.rm=T))%>%
    group_by(BBF_parfam)%>%
    summarise(pervote=mean(pervote,na.rm=T))%>%
    ungroup()
  # Compute across-group variance
  df_parfam_mean=left_join(df_parfam_mean,df_parfam_weight)
  df_parfam_variance=df_parfam_mean%>%
    group_by(variable)%>%
    summarise(gr_variance=wtd.var(value,pervote,na.rm=T))
  # Compute total variance
  df_tot_variance=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    group_by(variable)%>%
    summarise(tot_variance=var(value,na.rm=T))%>%
    ungroup()
  # Compute across-group variance/total variance
  temp=left_join(df_parfam_variance,df_tot_variance)%>%
    mutate(variance=gr_variance/tot_variance)%>%
    arrange(desc(variance)) %>%
    top_n(8)
  list_highbtwgrvar_sd=as.array(temp[[1]])
  return(list_highbtwgrvar_sd)
}

# Pick the variables, which distinguishes the most of Far-Right from others in wave X
create_varlist_distinct=function(df,wave_base,party_family){
  df_parfam_mean=df%>%
    filter(wave==wave_base)%>%
    select(-wave)%>%
    filter(!is.na(title))%>%
    mutate(BBF_parfam_dum=if_else(BBF_parfam==party_family,1,0))%>%
    group_by(BB_Country,BBF_parfam_dum,variable)%>%
    summarise(value=weighted.mean(value,pervote,na.rm=T))%>%
    group_by(BBF_parfam_dum,variable)%>%
    summarise(value=mean(value,na.rm=T))%>%
    ungroup()
  df_parfam_variance=df_parfam_mean%>%
    group_by(variable)%>%
    summarise(variance=var(value,na.rm=T))
  temp=df_parfam_variance%>%
    arrange(desc(variance)) %>%
    top_n(8)
  list_distinct=as.array(temp[[1]])
  return(list_distinct)
}

# Append Parfam
make_cw_parfam=function(raw_data_manifesto){
  ## get cw of manifesto-party-id and parfam
  cw_party_parfam=raw_data_manifesto%>%
    select(Y_manifesto_party_id,parfam,pervote)%>%
    group_by(Y_manifesto_party_id,parfam)%>%
    summarize(pervote=mean(pervote,na.rm=T))%>%
    ungroup()
  return(cw_party_parfam)
}


# Create standard deviation of Econ and Soc index in manifesto to standardize them in the plot
create_manifesto_std_all=function(raw_data_manifesto,list_var=NULL){
  df_manifesto_varsd=raw_data_manifesto%>%
    select(-pervote,-peruncod)
  # Add index
  df_manifesto_varsd=df_manifesto_varsd%>%
    mutate(econ_index=(per401 + per402 + per407 + per410 + per414 + per505 + per507 + per702) -
             (per403 + per404 + per405 + per406 + per409 + per412 + per413 + per415 + per504 + per506 + per701),
           soc_index=(per601 + per603 + per605 + per608 + per704) - (per201 + per202 + per503 + per602 + per604 + per607),
           intpeace_index= per102 + per105 + per106,
           trad_index = per603 - per604,
           soc_no_trad_index = soc_index - trad_index,
           rile_index=(per104 + per201 + per203 + per305 + per401 + per402 + per407 +
                         per414 + per505 + per601 + per603 + per605 + per606),
           hawk_dove_index = per101 + per106 + per107 + per108+ per103 + per105 - per109,
           hawk_dove_expanded_index = (per101 + per103 + per105 + per106 + per107 + per108) - (per102 + per104 +per109 +per110),
           neg_index = per102 + per105 + per109 + per110 + per204 + per407 + per602 + per604 + per608 + per702,
           eu_index = per108 - per110,
           gov_index = per303,
           gov_ex_index = per303 + per304)
  # Keep only econ, soc and hawk_dove (expanded)
  df_manifesto_varsd=df_manifesto_varsd%>%
    summarise(across(c("econ_index","soc_index", "trad_index", "soc_no_trad_index",
                       "hawk_dove_index", "hawk_dove_expanded_index",
                       "neg_index", "eu_index", "gov_index", "gov_ex_index", starts_with("per")),~sd(.x,na.rm=T)))
  df_manifesto_varsd=df_manifesto_varsd%>%
    pivot_longer(cols = everything(),names_to = "variable_original",values_to = "sd")
}



create_manifesto_std_all_ex=function(raw_data_manifesto,list_var=NULL){
  df_manifesto_varsd=raw_data_manifesto%>%
    select(-pervote,-peruncod)
  # Add index
  df_manifesto_varsd=df_manifesto_varsd%>%
    mutate(econ_index=(per401 + per402 + per407 + per410 + per414 + per505 + per507 + per702) -
             (per403 + per404 + per405 + per406 + per409 + per412 + per413 + per415 + per504 + per506 + per701),
           soc_index=(per601 + per603 + per605 + per608 + per704) - (per201 + per202 + per503 + per602 + per604 + per607),
           trad_index = per603 - per604,
           soc_no_trad_index = soc_index - trad_index,
           intpeace_index= per102 + per105 + per106,
           rile_index=(per104 + per201 + per203 + per305 + per401 + per402 + per407 +
                         per414 + per505 + per601 + per603 + per605 + per606),
           hawk_dove_index = per101 + per106 + per107 + per108+ per103 + per105 - per109,
           hawk_dove_expanded_index = (per101 + per103 + per105 + per106 + per107 + per108) - (per102 + per104 +per109 +per110),
           neg_index = per102 + per105 + per109 + per110 + per204 + per407 + per602 + per604 + per608 + per702,
           eu_index = per108 - per110,
           gov_index = per303,
           gov_ex_index = per303 + per304,
           owm_index = gender+age-minority-femShare,
           elite_index = education+marital_status+occupation-divorce)
  df_manifesto_varsd=df_manifesto_varsd%>%
    summarise(across(c("econ_index","soc_index", "trad_index", "soc_no_trad_index",
                       "hawk_dove_index", "hawk_dove_expanded_index",
                       "neg_index", "eu_index", "gov_index", "gov_ex_index", starts_with("per"),
                       "owm_index", "elite_index"),~sd(.x,na.rm=T)))
  df_manifesto_varsd=df_manifesto_varsd%>%
    pivot_longer(cols = everything(),names_to = "variable_original",values_to = "sd")
}

plot_box_parfam_static=function(data,list_var){
  df_long=data%>%
    select(BBF_parfam,variable,value)
  df_long=subset(df_long,variable %in% list_var)
  xmax=max(df_long$value,na.rm=T)
  xmin=min(df_long$value,na.rm=T)
  df_long=df_long%>%
    mutate(variable = factor(variable, levels = list_var))
  p=ggplot(df_long,aes(value,BBF_parfam)) +
    geom_boxplot(outlier.shape = NA)+
    geom_vline(xintercept = 0)+
    facet_wrap(~variable,nrow = 3, scales = "free")+
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"))+
    theme(plot.title = element_text(size=14),
          plot.subtitle = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size=14))
  return(p)
}

plot_distribution_parfam_static <- function(data,list_var, overlap = F, xlims = c(-2,2)) {
  # Keep survey weights with the voter-weight values used in the density.
  df_long <- data %>%
    select(BBF_parfam, variable, value, BB_Weight_CW) %>%
    filter(variable %in% list_var) %>%
    group_by(BBF_parfam,variable)
  if(n_distinct(df_long$BBF_parfam)== 2){
    fig_colors <- rev(c("#0072B2", "#E69F00" ))
    df_long <- df_long%>%
      mutate(variable = factor(variable, levels = list_var)) %>%
      ungroup() %>%
      mutate(BBF_parfam = factor(BBF_parfam, levels = sort(unique(BBF_parfam)), ordered = T))
  } else {
    fig_colors <- rev(c("#bc000b","#E69F00", "#0072B2", "#009E73"))
    df_long <- df_long%>%
      mutate(variable = factor(variable, levels = list_var)) %>%
      ungroup() %>%
      mutate(BBF_parfam = factor(BBF_parfam, levels = rev(sort(unique(BBF_parfam))), ordered = T))
  }
  p <- ggplot(df_long, aes(x = value, color = BBF_parfam, weight = BB_Weight_CW)) +

    theme_classic(base_size = 10)+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold", angle = 0))+
    theme(axis.title = element_text(),
          axis.text = element_text(),
          legend.title = element_text(),
          legend.text = element_text()) +
    scale_color_manual(name="",
                       values=fig_colors) +
    geom_vline(xintercept = 0, linetype="solid") +
    geom_hline(aes(yintercept=-Inf)) +
    coord_cartesian(clip="off") +
    scale_x_continuous(breaks = seq(-2,2,1),
                       labels = c(-2, -1, "0\nIndifferent", 1, 2), limits = xlims) +
    xlab("Weight") +
    ylab("Density")

  if(!overlap){
    p <- p +
      geom_density(position = "identity", size = 1) +
      facet_grid(BBF_parfam~variable  , scales = "fixed",
                 labeller = label_wrap_gen(width=20)) +
      theme(legend.position = "none")
  } else{
    p <- p +
      stat_density(geom = "line", position = "identity", size = 0.6) +
      facet_wrap(~variable  , scales = "fixed",
                 labeller = label_wrap_gen(width=20))+
      theme(legend.position = "bottom")
  }
  return(p)
}

plot_distribution_by_group <- function(data, list_var, group_var,
                                       static = T, overlap = F, wavenum = 6, xlims = c(-2,2)) {
  # Filter by wave and make long - "group variables" to be new names
  if(static){
    # Keep survey weights before reshaping group indicators.
    df_long <- data %>%
      filter(wave == wavenum) %>%
      select(variable_original, variable, value, BB_Weight_CW, all_of(group_var)) %>%
      pivot_longer(
        cols = all_of(group_var),
        names_to = c("group_variable"),
        values_to = "group_value") %>%
      select(-group_variable) %>%
      filter(variable %in% list_var) %>%
      group_by(group_value,variable) %>%
      mutate(variable = factor(variable, levels = list_var))
  } else {
    # Keep survey weights before reshaping group indicators.
    df_long <- data %>%
      filter(wave %in% wavenum) %>%
      select(wave, variable_original, variable, value, BB_Weight_CW, all_of(group_var)) %>%
      pivot_longer(
        cols = all_of(group_var),
        names_to = c("group_variable"),
        values_to = "group_value") %>%
      select(-group_variable) %>%
      filter(variable %in% list_var) %>%
      group_by(group_value,variable) %>%
      mutate(variable = factor(variable, levels = list_var)) %>%
      left_join(waveLabelsDF %>% mutate(waveNum= as.character(waveNum)), by = c("wave" = "waveNum"))
  }

  p <- ggplot(df_long, aes(x = value, weight = BB_Weight_CW)) +
    theme_classic(base_size = 10)+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold", angle = 0))+
    theme(axis.title = element_text(),
          axis.text = element_text(),
          legend.title = element_text(),
          legend.text = element_text()) +
    geom_vline(xintercept = 0, linetype="solid") +
    geom_hline(aes(yintercept=-Inf)) +
    coord_cartesian(clip="off") +
    scale_x_continuous(breaks = seq(-2,2,1),
                       labels = c(-2, -1, "0\nIndifferent", 1, 2), limits = xlims) +
    xlab("Weight") +
    ylab("Density")

  if(!overlap){
    p <- p + facet_grid(group_value ~ variable  , scales = "fixed",
                        labeller = label_wrap_gen(width=20))
  } else {
    p <- p + facet_wrap(~variable  , scales = "fixed",
                        labeller = label_wrap_gen(width=20))
  }

  if(static){
    if (n_distinct(df_long$group_value) == 3){
      fig_colors <- rev(c("#0072B2", "#E69F00", "#bc000b"))
    } else if(n_distinct(df_long$group_value) == 2) {
      fig_colors <- rev(c("#0072B2", "#E69F00"))
    } else{
      fig_colors <- ParFamColor
    }
    if(!overlap){
      p <- p + geom_density(position = "identity", size = 1, aes(color = group_value),
                            show.legend = F) +
        scale_color_manual(name="",
                           values = fig_colors) +
        theme(legend.position = "none")
    } else{
      p <- p + stat_density(position = "identity", geom = "line",
                            size = 0.6, aes(color = group_value)) +
        scale_color_manual(name="",
                           values = fig_colors) +
        theme(legend.position = "bottom")
    }
  } else {
    p <- p + stat_density(position = "identity", geom = "line",
                          size = 0.6, aes(color = waveLabels)) +
      scale_color_manual(name = "Wave",
                         values = c("#0072B2", "#E69F00", "#bc000b")) +
      scale_alpha_manual(name = "Wave",
                         values = c(0.6,1,0.6)) +
      theme(legend.position = "bottom")
  }
  return(p)
}

plot_errbar_parfam_static=function(data,list_var){
  if (data %>% pull(BBF_parfam) %>%  n_distinct() == 4) {
    fig_palette <- c("#D55E00","#E69F00", "#0072B2", "#009E73")
  }else {fig_palette <- c("#D55E00","#E69F00","#CC79A7" , "#0072B2", "#009E73")}
  df_long=data%>%
    select(BBF_parfam,variable,value,BB_Weight_CW)
  df_long=subset(df_long,variable %in% list_var)
  df_long=df_long%>%
    group_by(BBF_parfam,variable)%>%
    summarise(
      median=Hmisc::wtd.quantile(value, BB_Weight_CW, 0.5),
      pc75=Hmisc::wtd.quantile(value, BB_Weight_CW, 0.75),
      pc25=Hmisc::wtd.quantile(value, BB_Weight_CW, 0.25),
      .groups = "drop"
    )
  df_long=df_long%>%
    mutate(variable = factor(variable, levels = list_var))
  xmin=min(df_long$pc25,na.rm=T)
  xmax=max(df_long$pc75,na.rm=T)

  #p=ggplot(df_long,aes(x=median,y=BBF_parfam,color=BBF_parfam)) +
  p = ggplot(df_long,aes(y=BBF_parfam, color=BBF_parfam)) +
    #geom_point()+
    facet_wrap(~variable, scales = "free",ncol=2) +
    geom_boxplot(aes(xlower=pc25,xupper=pc75, xmiddle=median, x=median, xmin=pc25, xmax=pc75),
                 outlier.shape = NA, stat="identity", width=0.2)  +
    scale_y_discrete(limits = rev(levels(df_long$BBF_parfam))) +

    xlim(xmin,xmax)+
    theme_classic(base_size = 10)+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = "bold"))+
    theme(axis.title = element_text(),
          axis.text = element_text(),
          legend.title = element_text(),
          legend.text = element_text(),
          legend.position = "none")+
    scale_color_manual(name="",
                       values=fig_palette)+
    xlab("Voting Weights (p25, median, p75)")+
    ylab("") +
    geom_vline(xintercept = 0, linetype="dashed")


  #geom_errorbar(aes(xmin=pc25,xmax=pc75),width=.2)+


  return(p)
}

plot_dumbbell_parfam=function(raw_data_manifesto,raw_data_wvs,df_parfam,list_var){
  df_manifesto_stddev_wave6_listvar=create_manifesto_std(raw_data_manifesto,raw_data_wvs,list_var)
  df_parfam_mean_bywave=df_parfam%>%
    mutate(pervote=if_else(is.na(pervote),0,pervote))%>%
    group_by(wave,variable,BB_Country,BBF_parfam)%>%
    summarise(value=weighted.mean(value,pervote,na.rm=T))%>%
    ungroup()%>%
    group_by(wave,variable,BBF_parfam)%>%
    summarise(value=mean(value,na.rm=T))%>%
    ungroup()%>%
    filter(wave==4|wave==6)
  df_parfam_mean_bywave=subset(df_parfam_mean_bywave,variable %in% list_var)
  df_parfam_mean_bywave=left_join(df_parfam_mean_bywave,df_manifesto_stddev_wave6_listvar)
  df_parfam_mean_bywave=df_parfam_mean_bywave%>%
    mutate(value=value/sd)
  xmin=min(df_parfam_mean_bywave$value,na.rm=T)
  xmax=max(df_parfam_mean_bywave$value,na.rm=T)
  p=ggplot(df_parfam_mean_bywave,aes(x=value,y=BBF_parfam))+
    geom_line(aes(group=BBF_parfam))+
    geom_point(aes(color=wave,shape=wave),size=4)+
    facet_wrap(~variable,nrow = 3, scales = "free")+
    theme_classic()+
    xlim(xmin,xmax)+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"))+
    theme(plot.title = element_text(size=14),
          plot.subtitle = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size=14))
  return(p)
}

plot_dumbbell_variable=function(raw_data_manifesto,raw_data_wvs,df_parfam,list_var){
  df_manifesto_stddev_wave6_listvar=create_manifesto_std(raw_data_manifesto,raw_data_wvs,list_var)
  df_parfam_mean_bywave=df_parfam%>%
    mutate(pervote=if_else(is.na(pervote),0,pervote))%>%
    group_by(wave,variable,BB_Country,BBF_parfam)%>%
    summarise(value=weighted.mean(value,pervote,na.rm=T))%>%
    ungroup()%>%
    group_by(wave,variable,BBF_parfam)%>%
    summarise(value=mean(value,na.rm=T))%>%
    ungroup()%>%
    filter(wave==4|wave==6)
  df_parfam_mean_bywave=subset(df_parfam_mean_bywave,variable %in% list_var)
  df_parfam_mean_bywave=left_join(df_parfam_mean_bywave,df_manifesto_stddev_wave6_listvar)
  df_parfam_mean_bywave=df_parfam_mean_bywave%>%
    mutate(value=value/sd)
  xmin=min(df_parfam_mean_bywave$value,na.rm=T)
  xmax=max(df_parfam_mean_bywave$value,na.rm=T)
  p=ggplot(df_parfam_mean_bywave,aes(x=value,y=variable))+
    geom_line(aes(group=variable))+
    geom_point(aes(color=wave,shape=wave),size=4)+
    geom_vline(xintercept = 0)+
    facet_wrap(~BBF_parfam,nrow = 3, scales = "free")+
    theme_classic()+
    xlim(xmin,xmax)+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"))+
    theme(plot.title = element_text(size=14),
          plot.subtitle = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size=14))
  return(p)
}

plot_distribution_wave=function(raw_data_manifesto,raw_data_wvs,df_parfam,list_var,sample_parfam){
  # Restrict to the requested party family while preserving survey weights.
  df=df_parfam%>%
    filter(wave==3|wave==4|wave==6)%>%
    filter(BBF_parfam==sample_parfam)
  df=subset(df,variable %in% list_var)
  df=df%>%
    mutate(variable = factor(variable, levels = list_var))
  xmax=max(df$value,na.rm=T)
  xmin=min(df$value,na.rm=T)
  p=ggplot(df,aes(value, weight = BB_Weight_CW)) +
    geom_density(aes(color=as.factor(wave),linetype=as.factor(wave)),size=1.5)+
    geom_vline(xintercept = 0)+
    facet_wrap(~variable,nrow = 3, scales = "free")+
    theme_classic(base_size = 12)+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = "bold"))+
    scale_color_manual(name=paste0(sample_parfam,"\nWave"),
                       values=ParFamColor)+
    scale_linetype_manual(name=paste0(sample_parfam,"\nWave"),
                          values=c("solid","dotted","dashed"))+
    xlim(xmin,xmax)+
    ylim(0,5)
  return(p)
}

plot_distribution_wave_allpar=function(raw_data_manifesto,raw_data_wvs,df_parfam, list_var, differentLines=TRUE, onlyWave3=FALSE){
  # Restrict to plotted waves while preserving survey weights.
  df=df_parfam%>%
    filter(wave==3|wave==4|wave==6)
  df=subset(df,variable %in% list_var)
  df=df%>%
    mutate(variable = factor(variable, levels = list_var))
  xmax=max(df$value,na.rm=T)
  xmin=min(df$value,na.rm=T)
  labels <- raw_data_wvs%>%
    filter(BB_Wave_num %in% c(3,4,6))%>%
    arrange(BB_Wave_num)%>%
    select(waveLabels)%>%
    distinct()%>%
    pull()
  if (onlyWave3) {
    labels = c(labels[1], "", "")
  }

  colors <- ParFamColor[c(3,4,6) %in% unique(df_parfam$wave)]
  if (onlyWave3) {
    colors = c(colors[1], "white", "white")
  }
  alphas = c(0.6,1,0.6)
  if (onlyWave3) {
    alphas = c(1, 0 ,0)
  }

  rowNum = length(list_var)/2
  if (differentLines==TRUE) {
    lineTypes = c("twodash","solid")
  } else {
    lineTypes = rep("solid", 3)
  }

  p=ggplot(df,aes(value, weight = BB_Weight_CW)) +
    geom_density(aes(color=as.factor(wave),linetype=as.factor(wave)), show.legend = F,size=1.5)+
    stat_density(aes(color=as.factor(wave),linetype=as.factor(wave), alpha=as.factor(wave)),
                 geom = "line", position = "identity")+
    geom_vline(xintercept = 0)+
    facet_wrap(~variable,nrow = rowNum, scales = "fixed")+
    scale_color_manual(name=" Wave", values=colors, labels = labels) +
    scale_alpha_manual(name=" Wave",values=alphas, labels = labels) +
    scale_linetype_manual(name=" Wave", values=lineTypes, labels = labels) +
    scale_x_continuous(breaks = seq(-2,2,1), labels = c(-2, -1, "0\nindifferent", 1, 2), limits = c(xmin, xmax)) +
    labs(y="Density", x="Weight")
  return(p)
}

plot_distribution_wave_allpar_dashedline = function(raw_data_manifesto, labels, df_parfam, list_var, differentLines=TRUE, onlyWave3=FALSE){
  # Restrict to plotted waves while preserving survey weights.
  df = df_parfam %>%
    filter(wave==3|wave==4|wave==6)

  df=subset(df,variable %in% list_var)

  df = df %>%
    mutate(variable = factor(variable, levels = list_var))
  xmax=max(df$value,na.rm=T)
  xmin=min(df$value,na.rm=T)

  plot_labels <- labels


  colors <- ParFamColor[c(3,4,6) %in% unique(df_parfam$wave)]
  if (onlyWave3) {
    colors = c(colors[1], "white", "white")
  }

  alphas = c(0.6,1,0.6)

  if (onlyWave3) {
    alphas = c(1, 0 ,0)
  }

  rowNum = length(list_var)/2
  if (differentLines==TRUE) {
    lineTypes = c("solid", "longdash")
  } else {
    lineTypes = rep("solid", 2)
  }

  p = ggplot(df, aes(value, color = wave, linetype = wave, alpha = variable, weight = BB_Weight_CW)) +
    stat_density(geom = "line", position = "identity", size = 1) +
    geom_vline(xintercept = 0) +
    facet_wrap(~variable, nrow = rowNum, scales = "fixed") +
    scale_color_manual(values = colors, labels = plot_labels) +
    scale_alpha_manual(values = c(1, 1, 1), guide = 'none') +
    scale_linetype_manual(values = lineTypes, labels = plot_labels) +
    scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) +
    scale_x_continuous(breaks = seq(-2,2,1),
                       labels = c(-2, -1, "0\nIndifferent", 1, 2),
                       limits = c(-2, 2))+
    labs(y="Density", x="Weight") +
    theme_classic()
  theme(legend.title = element_blank(), legend.key.width = unit(32, "points"))
  return(p)
}

plot_distribution_wave_allpar_dashedline_threeindex = function(raw_data_manifesto, labels, df_parfam, list_var, differentLines=TRUE, onlyWave3=FALSE){
  # Restrict to plotted waves while preserving survey weights.
  df = df_parfam %>%
    filter(wave==3|wave==4|wave==6)

  df=subset(df,variable %in% list_var)

  df = df %>%
    mutate(variable = factor(variable, levels = list_var))
  xmax=max(df$value,na.rm=T)
  xmin=min(df$value,na.rm=T)

  plot_labels <- labels


  colors <- ParFamColor[c(3,4,6) %in% unique(df_parfam$wave)]
  if (onlyWave3) {
    colors = c(colors[1], "white", "white")
  }

  alphas = c(0.6,1,0.6)

  if (onlyWave3) {
    alphas = c(1, 0 ,0)
  }

  rowNum = length(list_var)/3
  if (differentLines==TRUE) {
    lineTypes = c("solid", "longdash")
  } else {
    lineTypes = rep("solid", 2)
  }

  p = ggplot(df, aes(value, color = wave, linetype = wave, alpha = variable, weight = BB_Weight_CW)) +
    stat_density(geom = "line", position = "identity", size = 1) +
    geom_vline(xintercept = 0) +
    facet_wrap(~variable, nrow = rowNum, scales = "fixed",
               labeller = labeller(variable = label_wrap_gen(width = 24))) +
    scale_color_manual(values = colors, labels = plot_labels) +
    scale_alpha_manual(values = c(1, 1, 1), guide = 'none') +
    scale_linetype_manual(values = lineTypes, labels = plot_labels) +
    scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) +
    scale_x_continuous(breaks = seq(-2,2,1),
                       labels = c(-2, -1, "0\nIndifferent", 1, 2),
                       limits = c(-2, 2))+
    labs(y="Density", x="Weight") +
    theme_classic()
  theme(legend.title = element_blank(), legend.key.width = unit(32, "points"))
  return(p)
}

prepare_weight_data = function(raw_data_manifesto, raw_data_wvs) {
  # Weight data
  ## Need to update
  raw_data = readRDS(file = paste0(model_results_path, "/weights.Rds"))


  # Missing variable when focusing only on east europe
  for (var in c("per415", "per505", "per602")) {
    if(! var %in% names(raw_data)){
      raw_data[[var]]=0
    }
  }

  ## Create index on weight data
  raw_data=raw_data%>%
    mutate(econ_index=(per401 + per402 + per407 + per410 + per414 + per505 + per507 + per702) -
             (per403 + per404 + per405 + per406 + per409 + per412 + per413 + per415 + per504 + per506 + per701),
           soc_index=(per601 + per603 + per605 + per608 + per704) - (per201 + per202 + per503 + per602 + per604 + per607))%>%
    na.omit()

  # Cut unnecessary data
  ## For ID variables: I NEED to match with WVS, we have BB_Id: Also I do not need Y_FR_Pop
  ## For weights (z): We only need econ and soc (no need for per)
  raw_data=raw_data%>%
    select(c("BB_Country",
             "Y_manifesto_party_id",
             "Y_FR_Pop",
             "id",
             "BB_Id",
             "wave",
             "econ_index",
             "soc_index"))

  # Make data to long
  df=pivot_longer(raw_data,
                  cols=!c("BB_Country",
                          "Y_manifesto_party_id",
                          "Y_FR_Pop",
                          "id",
                          "BB_Id",
                          "wave"),
                  names_to="variable_original",
                  values_to = "value")

  ###########################################
  # Standardization
  ###########################################
  # Append STD error
  ## First Get manifesto data std error with index
  df_manifesto=create_manifesto_std_all(raw_data_manifesto,raw_data_wvs)
  ## Then Merge them
  df=left_join(df,df_manifesto)
  # Standarize everything (multiply by sd)
  df=df%>%
    mutate(value=value*sd)%>%
    mutate(value=if_else(variable_original=="econ_index",value/16,value),
           value=if_else(variable_original=="soc_index",value/15,value))

  # Rename to labels by copying labels
  df=df %>%
    mutate(variable=if_else(variable_original=="econ_index",econIndexName,NA_character_),
           variable=if_else(variable_original=="soc_index",societyIndexName,variable))
  return(df)
}







# Scale all data with respect to a control group
prepapre_wvs_data=function(raw_data_wvs){
  ALL_DATA = as.data.table(raw_data_wvs)
  ALL_DATA = ALL_DATA[BB_Wave_num %in% c(3, 4, 6)]
  X_ops = ALL_DATA%>%
    select(id, starts_with("X_op"))
  ALL_DATA=ALL_DATA%>%
    select(BB_Wave_num,BB_Country,id,starts_with("X_dem"),X_general_self_political_scale)
  xNames = names(ALL_DATA)[grepl("X_", names(ALL_DATA))]
  opinionVars = names(ALL_DATA)[grepl("X_", names(ALL_DATA))]
  #opinionVars = opinionVars[! opinionVars %in% "X_op_confidence_parliament"]
  cat(paste0(opinionVars, ", "))

  # Standardize
  ALL_DATA[, eval(paste0(xNames, "S")) := lapply(.SD, scale), .SDcols = xNames]

  # Decide on sign
  directions = ALL_DATA[BB_Wave_num %in% c(3, 4, 6), sapply(.SD, function(x) sign(cor(x, X_general_self_political_scale))), .SDcols = paste0(opinionVars, "S")]

  # Create one ideology index based on opinions
  x = ALL_DATA %>%
    select(opinionVars)
  ALL_DATA[, opinionIndex := createStandardIndex(x, control=NULL, directions = directions)]
  ALL_DATA=ALL_DATA%>%
    select(BB_Wave_num,BB_Country,id,starts_with("X_"),opinionIndex) %>%
    mutate(wave=as.character(BB_Wave_num)) %>%
    inner_join(X_ops)
  return(ALL_DATA)
}

add_F_vars = function(data) {
  data <- data %>%
  mutate(
    F_sex = if_else(X_dem_male == 0, "Female", "Male"),
    F_age = if_else(X_dem_age >= 0 & X_dem_age <= 50, "18-50", NA_character_),
    F_age = if_else(X_dem_age > 50, "51+", F_age),
    F_emp = if_else(X_dem_employed == 1 | X_dem_employed == 0, "Not Full-time", NA_character_),
    F_emp = if_else(X_dem_employed == 2, "Full-time", F_emp),
    F_union = if_else(X_dem_labor_union_mem == 1, "Union Member", "Not a Union Member"),
    F_union = factor(F_union, levels = c("Union Member", "Not a Union Member")),
    F_edu = if_else(X_dem_education >= 17, "College", "Did Not Complete College"),
    F_bigtown = if_else(X_dem_log_town_size > log(100000), "Urban", "Rural"),
    F_bigtown = factor(F_bigtown, levels = c("Urban", "Rural")),
    F_PRRPscore_std_right=if_else(outcome_score_std>wtd.quantile(outcome_score_std, 0.5, weights = BB_Weight_CW), "Above Median PRRP Score", "Below Median PRRP Score"),
    F_income =  if_else(X_dem_Income_Decile>=5, "Above Median Income", "Below Median Income"))
    # F_rel = if_else(X_dem_rel_services == 0, "Not Attending Religious Services", "Attending Religious Services"))
}
