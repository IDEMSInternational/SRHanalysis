# Define package environment
utils::globalVariables(c("pkg_env"))
pkg_env <- new.env(parent = emptyenv())
pkg_env$rapidpro_key <- NULL
pkg_env$rapidpro_site <- NULL
pkg_env$rapidpro_uuid_names <- NULL

set_rapidpro_uuid_names <- function(uuid_names = get_flow_names()){
  pkg_env$rapidpro_uuid_names <- uuid_names
}

# aesthetics - removing _, and making first letter capital

naming_conventions <- function(x, replace, replace_after) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}

# wrapping text but retaining the levels
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# same function used in parent text
summary_calculation <- function(data = plhdata_org_clean, factors, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                                together = FALSE, include_margins = FALSE, drop = FALSE){
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    summary_output <- data %>%
      mutate(across(c({{ columns_to_summarise }}), ~ (as.character(.x)))) %>%
      group_by(across(c({{ columns_to_summarise }}, {{ factors }})), .drop = drop) %>%
      summarise(n = n(),
                perc = n()/nrow(.) * 100) %>%
      ungroup()
    if (include_margins){
      cts_margin <- data %>%
        group_by(across(c({{ columns_to_summarise }})), .drop = drop) %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)      
      ftr_margin <- data %>%
        group_by(across(c({{ factors }})), .drop = drop) %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)      
      corner_margin <- data %>%
        summarise(n = n(),
                  perc = n()/nrow(.) * 100)
      summary_output <- bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        #ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id %in% c(2, 4), "Total", .x))) %>%
        mutate(across({{ columns_to_summarise }}, ~ifelse(id %in% c(3, 4), "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        mutate(across({{ columns_to_summarise }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
    if (together){
      summary_output <- summary_output %>%
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
    }
  } else {
    summary_output <- data %>%
      group_by(across({{ factors }}), .drop = drop) %>%
      #mutate(across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      summarise(across({{ columns_to_summarise }}, ~mean(.x, na.rm = TRUE)))
    
    if (include_margins){
      corner_margin <- data %>%
        summarise(across(c({{ columns_to_summarise }}), ~mean(.x, na.rm  = TRUE)))
      
      summary_output <- bind_rows(summary_output, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id == 2, "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
  }
  if (length(data %>% dplyr::select({{ factors }})) == 1){
    cell_values_levels <- data %>% pull({{ factors }}) %>% levels()
    if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
    
    summary_output <- summary_output %>%
      dplyr::mutate(dplyr::across({{ factors }},
                                  ~ factor(.x))) %>%
      dplyr::mutate(dplyr::across({{ factors }},
                                  ~ forcats::fct_relevel(.x, cell_values_levels)))
    summary_output <- summary_output %>% dplyr::arrange({{ factors }})
  }
  if (length(data %>% dplyr::select({{ columns_to_summarise }})) == 1){
    cell_values_levels <- data %>% pull({{ columns_to_summarise }}) %>% levels()
    if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
    
    summary_output <- summary_output %>%
      dplyr::mutate(dplyr::across({{ columns_to_summarise }},
                                  ~ factor(.x))) %>%
      dplyr::mutate(dplyr::across({{ columns_to_summarise }},
                                  ~ forcats::fct_relevel(.x, cell_values_levels)))
    summary_output <- summary_output %>% dplyr::arrange({{ columns_to_summarise }})
  }
  return(unique(summary_output))
}

summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                          replace = "rp.contact.field.", include_margins = FALSE, wider_table = TRUE,
                          display_table = FALSE, naming_convention = TRUE, include_percentages = FALSE,
                          together = TRUE, drop = FALSE){
  
  summaries <- match.arg(summaries)
  
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      summaries = summaries,
                                      together = together,
                                      drop = drop)
  return_table_names <- naming_conventions(colnames(return_table), replace = replace)
  if (summaries == "mean"){
    if (naming_convention){
      colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
    }
  }
  if (display_table){
    if (summaries == "frequencies"){
      return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n)
    }

    return_table <- gt(as_tibble(return_table)) %>%
      tab_header(
        title = paste(return_table_names[1], "by", return_table_names[2])  # fix up. 
      ) %>%
      tab_style(locations = list(cells_body(columns = 1)),
                style = list(cell_borders(
                  sides = "right",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold"))) %>%
      tab_style(locations = list(cells_column_labels(columns = gt::everything())),
                style = list(cell_borders( 
                  sides = "bottom",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold")))
    #if (summaries == "mean"){
    #  names(return_table$`_data`) <- naming_conventions(names(return_table$`_data`), replace = replace)
    #}
  } else {
    if (summaries == "frequencies"){
      all_factors <- str_split(gsub("^c\\(|\\)$", "", deparse(substitute(factors))), pattern = ", ")
      all_columns_to_summarise <- str_split(gsub("^c\\(|\\)$", "", deparse(substitute(columns_to_summarise))), pattern = ", ")
      if (wider_table && !missing(columns_to_summarise) && (any(all_factors[[1]] %in% (all_columns_to_summarise)[[1]]) == FALSE)){
        if (together){
          values_from <- "Count (%)"
        } else {
          values_from <- "n"
        }
        return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = values_from, names_prefix = "")
      }
      if (naming_convention){
        colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
      }
    }
  }
  if ("Total" %in% colnames(return_table)){
    return_table <- return_table %>%
      relocate(Total, .after = last_col())
  }
  return(return_table)
}

flow_data_table_function <- function(flow_interaction, flow_name = NULL){
  if (!is.data.frame(flow_interaction)){
    flow_interaction <- plyr::ldply(flow_interaction) 
  }
  
  if (!is.null(flow_interaction$interacted)){
    flow_interaction$interacted <- ifelse(flow_interaction$interacted == TRUE, "Yes", "No")
    flow_interaction$interacted <- forcats::fct_expand(flow_interaction$interacted, c("Yes", "No"))
  } else {
    flow_interaction$interacted <- NA
  }
  flow_interaction_output <- flow_interaction %>%
      group_by({{ flow_name }}, interacted, .drop = FALSE) %>%
      summarise(Count = n()) %>% #, perc = round(n()/nrow(.)*100,2)) %>%
      #mutate("Count (%)" := str_c(`Count`, ' (', round(`perc`, 1), ")")) %>%
      #dplyr::select(-c(count, perc)) %>%
    map_df(rev)
  return(flow_interaction_output)
}

# Function based on http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# for plotting means and error bars
summarySE <- function(data=NULL, var, groups=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # Summary - vector with N, mean, and sd for each group var
  datac <- data %>%
    group_by(across({{ groups }})) %>%
    summarise(N = sum(!is.na({{ var }})),
              mean = mean({{ var }}, na.rm = na.rm),
              sd = sd({{ var }}, na.rm = na.rm))  
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#' * General level
#' *** Number of runs (proportions)
#' *** Number of interactions (responded==TRUE) (proportions)
#' * Result level
#' *** Rates of responses (corresponding to categories in wfr nodes) for a given result (specify name of result and categories)
#' *** The possibility to filter and summarise the information at flow level based on users/groups/contact field information

# General level - number of responses, category answers
response_rate_graphs<-function(flow_interaction, flow_name){
  #print(flow_interaction %>% group_by(response) %>% summarise(n())) 
  #print(flow_interaction %>% group_by(category) %>% summarise(n())) 
  #  ggplot(flow_interaction, aes(x = response)) +
  #    geom_bar() +
  #    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
  #  
  #  flow_interaction_response <- flow_interaction %>% filter(response == TRUE)
  #  ggplot(flow_interaction_response, aes(x = category)) +
  #    geom_bar() +
  #    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
}

create_user_dataframe <- function(flow_interaction){
  temp<-flow_interaction %>% group_by(uuid,response) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
  temp2<-flow_interaction %>% filter(response == TRUE) %>% group_by(uuid,category) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
}


# SRH data
srh_table_output <- function(flow_names = SRH_flow_names){
  srh_table <- NULL
  all_flow_names <- get_flow_names() %>% dplyr::select((name))
  for (i in flow_names){
    i_flow_names <- (all_flow_names %>% filter(grepl(i, name)))$name
    if (length(i_flow_names) != 0){
      i_flow <- get_flow_data(flow_name = i_flow_names)
      i_flow$`.id` <- (gsub(".*- ", "", i_flow$`.id`))
      srh_table[[which(flow_names == i)]] <- i_flow
      names(srh_table)[[which(flow_names == i)]] <- i
    }
  }
  return(srh_table)
}

flow_cat_frequency <- function(table = srh_data){
  freq_table <- NULL
  for (i in 1:length(table)){
    if (!is.null(table[[i]]) && nrow(table[[i]]) > 0){
      i_flow_n <- summary_table(table[[i]], factors = `.id`)
      i_flow_n$`.id` <- gsub(".*- ","", i_flow_n$`.id`)
      freq_table[[i]] <- i_flow_n
      names(freq_table)[[i]] <- names(table)[[i]]
    }
    #    } else {
    #      print("no")
  }
  return(freq_table)
}

summary_plot <- function(data = plhdata_org_clean, columns_to_summarise, naming_convention = TRUE, replace = "rp.contact.field.",
                         replace_after = NULL,
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data%>%select(.data[[columns_to_summarise]])), replace = replace, replace_after = replace_after)	
  
  return_plot <- ggplot(data) +	
    viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +	
    labs(x = x_axis_label, y = "Count") +	
    theme_classic()	
  
  if(plot_type == "histogram"){
    return_plot <- return_plot + geom_histogram(data = data, aes(x = .data[[columns_to_summarise]]), stat = "count")
  } else {
    return_plot <- return_plot + geom_boxplot(data = data, aes(y = .data[[columns_to_summarise]]))
  }
  
  return(return_plot)	
}

naming_conventions <- function(x, replace, replace_after) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}
