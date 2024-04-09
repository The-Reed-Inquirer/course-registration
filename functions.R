library(tidyverse)
library(RSelenium)
library(rvest)
library(ggplot2)
library(reticulate)

retrieve_text_from_subselector <- function(elem,subselector) {
  sub_elem <- elem$findChildElements("css selector",subselector)
  if (length(sub_elem) > 0) {
    sub_elem <- sub_elem[[1]]
    value <- unlist(sub_elem$getElementText())
    return(value)
  } else {
    return(NA)
  }
}

process_course <- function(course_elem) {
  title <- retrieve_text_from_subselector(course_elem,".course-header>.d-flex>.course-title")

  basics_wrapper <- course_elem$findChildElements("css selector",".course-basics")[[1]]

  instructor <- retrieve_text_from_subselector(basics_wrapper,".course-instructor>dd")
  days <- retrieve_text_from_subselector(basics_wrapper,".course-days>dd")
  time <- retrieve_text_from_subselector(basics_wrapper,".course-time>dd")
  enrolled <- retrieve_text_from_subselector(basics_wrapper,".course-enrolled>dd")
  units <- retrieve_text_from_subselector(basics_wrapper,".course-units>dd")
  mode <- retrieve_text_from_subselector(basics_wrapper,".course-mode>dd")

  section_info <- retrieve_text_from_subselector(course_elem,".course-header>.d-flex>.course-section-info")

  result <- c(title,instructor,days,time,enrolled,units,mode,section_info)

  return(result)
}

transform_ls_ls <- function(ls) {
  max_cols <- max(unlist(lapply(ls,length)))
  max_rows <- length(ls)
  df <- as.data.frame(matrix(nrow=max_rows,ncol=max_cols))
  for (i in 1:dim(df)[1]) {
    df[i,] <- ls[[i]]
  }
  return(df)
}

process_page <- function() {
  search_results <- remDr$findElement("css selector","#search-results-inner")
  course_list <- search_results$findChildElements("css selector",".course")

  superlist <- lapply(course_list,process_course)
  df <- transform_ls_ls(superlist)

  colnames(df) <- c("Title","Instructor","Days","Time","Enrolled","Units","Mode","Type")

  return(df)
}

write_data <- function(index) {
  df <- process_page()
  write.csv(df,paste("data/",index,".csv",sep=""))
}

bind_folder <- function(directory) {
  all_files <- list.files(directory)
  all_files <- paste(directory,"/",all_files,sep="")
  df <- read_csv(all_files[1])
  for (i in 2:length(all_files)) {
    temp_df <- read_csv(all_files[i])
    df <- rbind(df,temp_df)
  }
  return(df)
}

freq_table <- function(vector) {
  sorts <- unique(vector)
  df <- as.data.frame(matrix(ncol=2,nrow=length(sorts)))
  colnames(df) <- c("Value","N")
  df$Value <- sorts
  for (i in 1:length(df$Value)) {
    df[i,]$N <- length(which(vector == df[i,]$Value))
  }
  return(df)
}

pretty_split <- function(vec,separator) {
  ls <- str_split(vec,separator)
  ls <- lapply(ls,str_trim)
  return(ls)
}

pull_sub_index <- function(ls,index) {
  result <- c()
  for (x in ls) {
    result <- c(result,x[index])
  }
  return(result)
}

sort_and_apply <- function(df,sort_col,assess_col,func) {
  sorts <- unique(df[[{{sort_col}}]])
  result <- as.data.frame(matrix(ncol=2,nrow=length(sorts)))
  colnames(result) <- c("Values","Output")
  result$Values <- sorts
  for (i in 1:length(result$Values)) {
    curr_sort <- result[i,]$Values
    temp_df <- df[(which(df[[{{sort_col}}]] == curr_sort)),]
    result[i,]$Output <- func(temp_df[[{{assess_col}}]])
  }
  return(result)
}

unique_value_tester <- function(vec) {
  return(length(unique(vec)))
}

soft_parse_number <- function(vec) {
  for (i in 1:length(vec)) {
    if (is.character(vec[i]))
      vec[i] <- parse_number(vec[i])
  }
  return(vec)
}

apply_na_rm <- function(fun) {
  result_func <- function(vec) {
    return(fun(vec,na.rm=TRUE))
  }
  return(result_func)
}

recursive_join <- function(vec,by_col) {
  result <- vec[1]
  for (i in 2:length(vec)) {
    message(vec[i])
    result <- left_join(result,vec[i],by=by_col)
  }
  return(result)
}

fill_by_level <- function(depar,df,calc_func) {
  result <- sort_and_apply(df %>% filter(Department == depar),"Level","Students",apply_na_rm(calc_func)) %>% arrange(Values)
  colnames(result) <- c("Level","Students")
  result <- cbind(Department=depar,result)
  return(result)
}

list_join <- function(all_tibbles) {
  df <- all_tibbles[[1]]
  if (length(all_tibbles) > 1) {
    for (i in 2:length(all_tibbles)) {
      df <- rbind(df,all_tibbles[[i]])
    }
  }
  return(df)
}

freqs_table <- function(vec) {
  unique_vals <- unique(vec)
  result <- as.data.frame(matrix(ncol=2,nrow=length(unique_vals)))
  colnames(result) <- c("Val","Count")
  result$Val <- unique_vals
  for (i in 1:length(result$Val)) {
    if (is.na(result[i,]$Val)) {
      result[i,]$Count <- length(which(is.na(vec)))
    } else {
      result[i,]$Count <- length(which(vec == result[i,]$Val))
    }
  }
  result <- result %>% arrange(Val)
  return(result)
}

sort_and_tibblate <- function(df,sort_cols_ls,value_col,tibblate_func) {
  sort_col <- sort_cols_ls[1]
  unique_sorts <- unique(df[[{{sort_col}}]])
  tibbles_ls <- list()

  for (i in 1:length(unique_sorts)) {
    temp_df <- df[which(df[[{{sort_col}}]] == unique_sorts[i]),]

    if (length(sort_cols_ls) == 1) {
      tibble_output <- tibblate_func(temp_df[[{{value_col}}]])
    } else {
      tibble_output <- sort_and_tibblate(temp_df,sort_cols_ls[2:length(sort_cols_ls)],value_col,tibblate_func)
    }

    tibble_output <- cbind(SORT_COLUMN=unique_sorts[i],tibble_output)
    tibble_output <- tibble_output %>% arrange(SORT_COLUMN)
    colnames(tibble_output)[which(colnames(tibble_output) == 'SORT_COLUMN')] <- sort_col
    tibbles_ls[[i]] <- tibble_output
  }

  result <- list_join(tibbles_ls)
  return(result)
}

one_value_df <- function(func_to_apply) {
  output_func <- function(vec) {
    val <- func_to_apply(vec)
    result <- as.data.frame(val)
    colnames(result) <- c("VALUE")
    return(result)
  }
  return(output_func)
}

non_na_values <- function(vec) {
  filtered_vec <- vec[!is.na(vec)]
  return(length(filtered_vec))
}

unique_vals <- function(vec) {
  return(length(unique(vec)))
}

evaluate_py_list <- function(ls) {
  return(reticulate::py_eval(ls))
}

col_rename <- function(df,original,new) {
  colnames(df)[which(colnames(df) == original)] <- new
  return(df)
}

py_ls_multifilter <- function(df,col,val) {
  expanded_lists <- lapply(df[[{{col}}]],evaluate_py_list)
  valid_indices <- c()
  for (i in 1:length(expanded_lists)) {
    if (val %in% expanded_lists[[i]]) {
      valid_indices <- c(valid_indices,i)
    }
  }
  return(df[valid_indices,])
}
