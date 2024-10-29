library(knitr)  
library(kableExtra)
library(tidyverse)
library(magrittr)

strawberry <- read_csv("E:/Desktop/BU/2024 Fall/MA615/Strawberry/Straw_new/strawberries25_v3.csv", col_names = TRUE, show_col_types = FALSE )

strawberry <- strawberry |> 
  filter(`Geo Level`== "NATIONAL" | `Geo Level`== "STATE")


drop_one_value_col <- function(df, prt_val = FALSE){ 
  # browser()
  df_id <- ensym(df)
  if(prt_val){
    msg = paste("Looking for single value columns in data frame: ",as.character(df_id) )
    print(msg)}
  ## takes whole dataframe
  dropc <- NULL
  val <- NULL
  ## test each column for a single value
  for(i in 1:dim(df)[2]){   
    if(dim(distinct(df[,i]))[1] == 1){
      dropc <- c(dropc, i)
      val <- c(val, df[1,i])
    }
  } 
  
  if(prt_val){
    if(is.null(dropc)){
      print("No columns dropped")
      return(df)}else{
        print("Columns dropped:")
        # print(colnames(df)[drop])
        print(unlist(val))
        df <- df[, -1*dropc]
        return(df)
      }
  }
  df <- df[, -1*dropc]
  return(df)
}


## use the function

strawberry <- strawberry |> drop_one_value_col(prt_val = FALSE)

straw_cen <- strawberry |> filter(Program=="CENSUS")

straw_sur <- strawberry |> filter(Program=="SURVEY")

straw_cen <- straw_cen |> drop_one_value_col()

straw_sur <- straw_sur |> drop_one_value_col()


straw_sur1 <- straw_sur |>  separate_wider_delim(cols = `Data Item`,
                                                 delim = ", ",
                                                 names = c("straw",
                                                           "mkt",
                                                           "measure",
                                                           "other"
                                                 ),
                                                 too_many = "merge",
                                                 too_few = "align_start")


straw_sur2 <- straw_sur1 |> separate_wider_delim(cols = "straw", 
                                                 delim = " - ",
                                                 names = c("straw",
                                                           "more"),
                                                 too_many = "merge",
                                                 too_few = "align_start"
)

shift_loc <- function(df, col_name, dat_name, num_col, num_shift){
  # browser()
  col_num = which(colnames(df) == col_name)
  row_num = which(df[,col_num] == dat_name)  ## calcs a vector of rows
  
  for(k in 1:length(row_num)){
    d = rep(0,num_col) ## storage for items to be moved
    for(i in 1:num_col){
      d[i] = df[row_num[k], col_num + i - 1]
    }
    for(i in 1:num_col){
      ra = row_num[k]
      cb = col_num + i - 1
      df[ra, cb] <-  NA
    }
    for(j in 1:num_col){
      rc = row_num[k]
      cd = col_num + j - 1 + num_shift
      df[rc, cd] = d[j]
    }
  }
  # sprintf("Rows adjusted:")
  # print("%d",row_num)
  return(df)
}

straw_sur2 %<>% shift_loc("more", "PRICE RECEIVED", 2, 1 )

straw_sur2 %<>% shift_loc("more", "ACRES HARVESTED", 1, 1 )

straw_sur2 %<>% shift_loc("more", "ACRES PLANTED", 1, 1 )

straw_sur2 %<>% shift_loc("more", "PRODUCTION", 2, 1 )

straw_sur2 %<>% shift_loc("more", "YIELD", 2, 1 )

straw_sur2 %<>% shift_loc("more", "APPLICATIONS", 3, 1 )

straw_sur2 %<>% shift_loc("more", "TREATED", 3, 1 )

straw_sur2 %<>% drop_one_value_col()


straw_sur2 <- straw_sur2 |>  
  separate_wider_delim(cols = Domain,
                       delim = ", ",
                       names = c("col1",
                                 "col2"),
                       
                       too_many = "merge",
                       too_few = "align_start")


# unique(straw_sur2$col1)

survey_d_total <- straw_sur2 |>  filter(col1 == "TOTAL")

survey_d_chem <- straw_sur2 |>  filter(col1 == "CHEMICAL")

survey_d_fert <- straw_sur2 |>  filter(col1 == "FERTILIZER")


# survey_d_total %<>% drop_one_value_col()
# 
# ### align terms
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN $ / CWT", 1, 1 )
# 
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN $", 1, 1 )
# 
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN CWT", 1, 1 )
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN TONS", 1, 1 )
# 
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN CWT / ACRE", 1, 1 )
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN TONS / ACRE", 1, 1 )
# 
# 
# #### split the mkt column
# 
# 
# survey_d_total <- survey_d_total |>  
#   separate_wider_delim(cols = mkt,
#                        delim = " - ",
#                        names = c("col3",
#                                  "col4"),
#                        too_many = "merge",
#                        too_few = "align_start")
# 
# 
# survey_d_total %<>% drop_one_value_col()
# 
# ### align terms
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN $ / CWT", 1, 1 )
# 
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN $", 1, 1 )
# 
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN CWT", 1, 1 )
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN TONS", 1, 1 )
# 
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN CWT / ACRE", 1, 1 )
# 
# survey_d_total %<>% shift_loc("measure", "MEASURED IN TONS / ACRE", 1, 1 )
# 
# 
# #### split the mkt column
# 
# 
# survey_d_total <- survey_d_total |>  
#   separate_wider_delim(cols = mkt,
#                        delim = " - ",
#                        names = c("col3",
#                                  "col4"),
#                        too_many = "merge",
#                        too_few = "align_start")
# 
# 

survey_d_chem <- survey_d_chem |> drop_one_value_col()

survey_d_chem <- survey_d_chem |> select(-`State ANSI`)

## California Chemicals

# ca_chem <- survey_d_chem |> filter(State=="CALIFORNIA") |>
#   select()

survey_d_chem1 <- survey_d_chem |>  
  separate_wider_delim(cols = mkt,
                       delim = " - ",
                       names = c("mk1",
                                 "mk2"),
                       too_many = "merge",
                       too_few = "align_start")


survey_d_chem1$measure <- str_replace(survey_d_chem1$measure, "MEASURED IN ", "")

# unique(survey_d_chem$`Domain Category`)

survey_d_chem1$`Domain Category` <- str_replace(survey_d_chem1$`Domain Category`, "CHEMICAL, ", "")

survey_d_chem1 <- survey_d_chem1 |> rename(chem = `Domain Category`)

survey_d_chem1 <- survey_d_chem1 |>
  separate_wider_delim(cols = chem,
                       delim = ": ",
                       names = c("type",
                                 "chem_type"),
                       too_many = "merge",
                       too_few = "align_start")

# s1 <- survey_d_chem$col2 == survey_d_chem$type
# sum(s1)

survey_d_chem1 <- survey_d_chem1 |> select(-col2)

survey_d_chem1 <- survey_d_chem1 |> 
  rename(chem_name = chem_type)

survey_d_chem1$chem_name <- str_replace(survey_d_chem1$chem_name, "^\\(", "")

survey_d_chem1$chem_name <- str_replace(survey_d_chem1$chem_name, "\\)$", "")

survey_d_chem1 <- survey_d_chem1 |>  
  separate_wider_delim(cols = chem_name,
                       delim = " = ",
                       names = c("chem_name",
                                 "chem_index"),
                       too_many = "error",
                       too_few = "align_start")

# write.csv(survey_d_chem1, "E:/Desktop/BU/2024 Fall/MA615/Strawberry/Straw_new.csv", row.names = FALSE)

# If you want to save it as a separate dataset, you can write it to a new CSV file

survey_d_chem1$Value[survey_d_chem1$Value == "(D)"] <- 0

# Convert the 'Value' column to numeric (in case it's currently a character type)
survey_d_chem1$Value <- as.numeric(survey_d_chem1$Value)

# Remove rows where 'Value' is NA
survey_d_chem1 <- survey_d_chem1 %>%
  filter(!is.na(Value))

# View the cleaned data
print(head(survey_d_chem1))

subset_data <- survey_d_chem1 %>%
  filter(measure == "LB / ACRE / APPLICATION")

#write.csv(subset_data, "E:/Desktop/BU/2024 Fall/MA615/Strawberry/filtered_subset.csv", row.names = FALSE)

########################### EDA ################################

##### Only contain three catog

filtered_data <- subset_data %>%
  filter(type != "OTHER")

# Group the filtered data by Year, State, and Type, and summarize the Value column
grouped_filtered_data <- filtered_data %>%
  group_by(Year, State, type) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE))

# Create a ggplot to visualize the total Value by Year, State, and Type (excluding "OTHER")
ggplot(grouped_filtered_data, aes(x = Year, y = Total_Value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ State) + 
  labs(title = "Total Chemical Value by Year, State, and Type (excluding 'OTHER')",
       x = "Year",
       y = "Total Value",
       fill = "Chemical Type") +
  theme_minimal()

###### All type

grouped_data <- subset_data %>%
  group_by(Year, State, type) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE))

# Create a ggplot to visualize the total Value by Year, State, and Type
ggplot(grouped_data, aes(x = Year, y = Total_Value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ State) + 
  labs(title = "Total Chemical Value by Year, State, and Type",
       x = "Year",
       y = "Total Value",
       fill = "Chemical Type") +
  theme_minimal()

########### Year by value

df_year <- filtered_data

df_year$Value <- as.numeric(as.character(df_year$Value))
cleaned_data <- df_year %>% filter(!is.na(Value))

# Summarize chemical usage by year and type
chemical_usage_by_year <- cleaned_data %>%
  group_by(Year, type) %>%
  summarise(Total_Usage = sum(Value, na.rm = TRUE))

# Plotting the trends in chemical usage over time
ggplot(chemical_usage_by_year, aes(x = Year, y = Total_Usage, color = type, group = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Trends in Chemical Usage Over Time",
       x = "Year", y = "Total Chemical Usage",
       color = "Chemical Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

############### pie chart

type_proportion <- filtered_data %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(proportion = (count / sum(count)) * 100)

# Plotting the pie chart
ggplot(type_proportion, aes(x = "", y = proportion, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Each Chemical Type", fill = "Chemical Type") +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 15))
