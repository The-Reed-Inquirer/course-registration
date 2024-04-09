source("functions.R")

# simple data cleaning of solar data

df <- read_csv("data_mar22.csv")

ident_list <- pretty_split(df$Title," - ")

df <- cbind(Section_ID=NA,df)

df <- cbind(Name=NA,df)

df$Section_ID <- pull_sub_index(ident_list,1)

df$Name <- pull_sub_index(ident_list,2)

section_list <- str_split(df$Section_ID," ")

df <- cbind(Section=NA,df)
df$Section <- pull_sub_index(section_list,3)

df <- cbind(Number=NA,df)
df$Number <- pull_sub_index(section_list,2)

df <- cbind(Department=NA,df)
df$Department <- pull_sub_index(section_list,1)

#load in course catalog data

catalog <- read_csv("catalog.csv")

colnames(catalog)[which(colnames(catalog) == 'course')] <- 'Section_ID'

df <- left_join(df,catalog,by='Section_ID')

df <- df %>% col_rename('Name','Name_SOLAR') %>% col_rename('Title','Title_Catalog')

df <- df %>% filter(Department != "PE")

df$Number <- parse_number(df$Number)
df$Units <- parse_number(df$Units)

df <- cbind(df,Students=NA)
df <- cbind(df,Seats=NA)

fractional_indices <- which(grepl(" of ",df$Enrolled))
split_enrolled <- pretty_split(df$Enrolled," of ")

for (i in fractional_indices) {
  df[i,]$Students <- split_enrolled[[i]][1]
  df[i,]$Seats <- split_enrolled[[i]][2]
}

df$Students <- parse_number(df$Students)
df$Seats <- parse_number(df$Seats)

df <- cbind(Class_ID=NA,df)
df$Class_ID <- paste(df$Department,df$Number,df$Name)

test <- sort_and_apply(df,"Class_ID","Seats",unique_value_tester)

df <- df %>% filter(!grepl("CANCELLED",Type))

unique_ids <- unique(df$Class_ID)

for (x in unique_ids) {
  indices <- which(df$Class_ID == x)
  temp_df <- df[indices,]
  if (length(unique(temp_df$Seats)) == 2) {
    if (is.na(df[indices[1],]$Seats)) {
      df[indices[1],]$Seats <- df[indices[2],]$Seats
    }
    if (is.na(df[indices[2],]$Seats)) {
      df[indices[2],]$Seats <- df[indices[1],]$Seats
    }
  }
}

df <- cbind(df,Percent_Fill=NA)

full_indices <- which(grepl("Full",df$Enrolled))

df[full_indices,]$Percent_Fill <- 1

for (i in 1:length(df$Percent_Fill)) {
  if (is.na(df[i,]$Percent_Fill)) {
    df[i,]$Percent_Fill <- df[i,]$Students / df[i,]$Seats
  }
}

for (j in 1:length(df$Students)) {
  if (is.na(df[j,]$Students) && !is.na(df[j,]$Percent_Fill && !is.na(df[j,]$Seats))) {
    df[j,]$Students <- df[j,]$Seats * df[j,]$Percent_Fill
  }
}

df <- cbind(df,Level=NA)
df$Level <- df$Number %/% 100

write.csv(df,"df.csv",row.names=FALSE)

#conclude data processing

#app preparation

department_students <- sort_and_apply(df,"Department","Students",apply_na_rm(median))
write.csv(department_students,"course-calculator/department_students.csv",row.names=FALSE)

level_students <- sort_and_apply(df,"Level","Students",apply_na_rm(median))
colnames(level_students) <- c("Level","Typical for Level")
write.csv(level_students,"course-calculator/level_students.csv",row.names=FALSE)

df <- cbind(df,Depar_Level_ID=NA)
df$Depar_Level_ID <- paste(df$Department,"_",df$Level,sep="")
depar_level_students <- sort_and_apply(df,"Depar_Level_ID","Students",apply_na_rm(median))
colnames(depar_level_students) <- c("Depar_Level_ID","Typical for Department & Level")
write.csv(depar_level_students,"course-calculator/depar_level_students.csv",row.names=FALSE)

#journalistic analysis

department_fill <- sort_and_apply(df,"Department","Percent_Fill",apply_na_rm(median))
colnames(department_fill) <- c("DEPAR","Perc_Fill")

department_students <- sort_and_apply(df,"Department","Students",apply_na_rm(median))
colnames(department_students) <- c("DEPAR","Students")

department_total_students <- sort_and_apply(df,"Department","Students",apply_na_rm(sum))
colnames(department_total_students) <- c("DEPAR","Total_Students")

department_num_classes <- sort_and_apply(df,"Department","Students",length)
colnames(department_num_classes) <- c("DEPAR","Num_Classes")

depar_table <- left_join(department_fill,department_students,by="DEPAR")
depar_table <- left_join(depar_table,department_total_students,by="DEPAR")
depar_table <- left_join(depar_table,department_num_classes,by="DEPAR")

depar_table <- depar_table %>%
  filter(Num_Classes > 0) %>%
  filter(Total_Students > 0) %>%
  arrange(-Students)

depar_tibbles <- lapply(unique(df$Department),fill_by_level,df,median)

depar_full_table <- list_join(depar_tibbles)
depar_full_table$Level <- depar_full_table$Level * 100

depar_all_stats_table <- sort_and_apply(depar_full_table,"Level","Students",apply_na_rm(median)) %>% arrange(Values)
colnames(depar_all_stats_table) <- c("Level","Students")
depar_all_stats_table <- cbind(Department="All Departments",depar_all_stats_table)

depar_full_table <- rbind(depar_all_stats_table,depar_full_table)

depar_histograms <- sort_and_tibblate(df,c('Department','Level'),'Students',freqs_table)
all_depar_histogram <- sort_and_tibblate(df,c('Level'),'Students',freqs_table)
all_depar_histogram <- cbind(Department='All Departments',all_depar_histogram)

depar_histograms <- rbind(all_depar_histogram,depar_histograms)

depar_histograms$Level <- depar_histograms$Level * 100

depar_histograms <- depar_histograms %>% filter(Val < 48)

write.csv(depar_histograms,"depar_histograms.csv",row.names=FALSE)

#by group analysis

coded_groups <- df
coded_groups <- cbind(GROUP_CODE=NA,coded_groups)
coded_groups$GROUP_CODE <- "ALL"

poss_groups <- c('1','2','3','3+')
for (curr_group in poss_groups) {
  temp <- df %>% py_ls_multifilter('group',curr_group)
  temp <- cbind(GROUP_CODE=curr_group,temp)
  coded_groups <- rbind(coded_groups,temp)
}

primary_groups <- coded_groups %>% filter(GROUP_CODE != "ALL") %>% arrange(parse_number(GROUP_CODE))

write.csv(primary_groups,"primary_groups.csv",row.names=FALSE)

group_load_by_level <- sort_and_tibblate(primary_groups,c('GROUP_CODE','Level'),'Students',one_value_df(apply_na_rm(median)))
colnames(group_load_by_level) <- c("GROUP_CODE","Level","Med_Students")
group_load_by_level <- cbind(Div_ID=paste(group_load_by_level$GROUP_CODE,"_",group_load_by_level$Level,sep=""),group_load_by_level)

group_load_num_sections <- sort_and_tibblate(primary_groups,c('GROUP_CODE','Level'),'Students',one_value_df(length))
colnames(group_load_num_sections) <- c("GROUP_CODE","Level","Num_Sections")
group_load_num_sections <- cbind(Div_ID=paste(group_load_num_sections$GROUP_CODE,"_",group_load_num_sections$Level,sep=""),group_load_num_sections)

group_load_non_na_sections <- sort_and_tibblate(primary_groups,c('GROUP_CODE','Level'),'Students',one_value_df(non_na_values))
colnames(group_load_non_na_sections) <- c("GROUP_CODE","Level","Data_Sections")
group_load_non_na_sections <- cbind(Div_ID=paste(group_load_non_na_sections$GROUP_CODE,"_",group_load_non_na_sections$Level,sep=""),group_load_non_na_sections)

group_load_by_level <- left_join(group_load_by_level,group_load_non_na_sections %>% select(!GROUP_CODE) %>% select(!Level),by="Div_ID")
group_load_by_level <- left_join(group_load_by_level,group_load_num_sections %>% select(!GROUP_CODE) %>% select(!Level),by="Div_ID")

group_load_by_level$Level <- group_load_by_level$Level * 100
group_load_by_level$GROUP_CODE <- paste("Group ",group_load_by_level$GROUP_CODE,sep="")

group_load_by_level <- cbind(group_load_by_level,Data_Accuracy=NA)
group_load_by_level$Data_Accuracy <- group_load_by_level$Data_Sections / group_load_by_level$Num_Sections

write.csv(group_load_by_level,"group_load_by_level.csv",row.names=FALSE)

staff_by_level <- sort_and_tibblate(primary_groups,c('GROUP_CODE','Level'),'Instructor',one_value_df(unique_vals))
colnames(staff_by_level) <- c("GROUP_CODE","Level","Faculty")
staff_by_level <- cbind(Div_ID=NA,staff_by_level)


#write.csv(depar_full_table,"depar_table_flourish.csv",row.names=FALSE)
