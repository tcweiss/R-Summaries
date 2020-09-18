
###################################
#
# THIS SCRIPT INCLUDES USEFUL FUNCTIONS 
# FOR CLEANING AND ANALYZING DATA
#
####################################



############
# CLEANING
###########

# MANUAL RECODING ----------------------------------------------------------------

# Can be used to change specific observations within a variable.
# Works best together with mutate.
# .default refers to values that are not mentioned in specific arguments.

data %>% 
  mutate(new_variable = recode(old_variable, 
                               `old_entry1` = "new_entry1",
                               `old_entry2` = "new_entry2",
                               .default = "all_other"


# Can also be used to recode to factors.

data %>% 
  mutate(new_variable = recode_factor(old_variable, 
                               `old_entry1` = "new_entry1",
                               `old_entry2` = "new_entry2",
                               .default = "all_other"
  ))



# AUTOMATIC RECODING ------------------------------------------------------

# Can be used to change values if they fall in a predefined pattern.
# The first argument is the column and a condition to be tested.
# The second is assigned to a value if the conditon is true, the third if false.
# Works well with mutate.

data %>% 
  
  mutate(column1 = if_else(
    
    between(column2, 100, 200), "ifTrue", "ifFalse")
    
    )


# Works like if_else, only that several conditions and assignment values can be defined.
# If no condition applies, the value next to TRUE will be assigned as a default value.

data %>% 
  
  mutate(column1 = case_when(
    
    between(column2, 100, 200) ~ "ifTrue",
    between(column2, 201, 300) ~ "ifTrue",
    TRUE ~ "gen_z"
    
  ))


# RESHAPING ---------------------------------------------------------------

# Can be used to convert "wide" to "long".
# key indicates the name of the new variable which includes the column names of the old variables.
# value indicates the name of the new variable which includes the observations of the old variables.
# The third argument indicates the variables from the old dataframe to be used.

gather(key = "NewVarName1", value = "NewVarName2", Var1:Va5, factor_key = TRUE, na.rm = TRUE)


# Can be used to convert "long" to "wide".
# key indicates the name of the column whose observations should be used as new column names.
# value indicates the name of the column whose observations should be used as new column content.
# 

spread(key = OldVarName, value = NewVarName, Var1:Var5, convert = TRUE)





# SEPARATING --------------------------------------------------------------

# Can be used to split one column into two, based on a given symbol in each line.
# col indicates the old variable to split. 
# into indicates the names of the resulting variables.
# If the last part should simply be dropped, indicate only one variable at into = and add extra = "drop".
# sep indicates the symbol which should be used to split the columns.


separate(col = OldVar, into = c("NewVar1", "NewVar2"), extra = "drop", sep = "separator symbol")



# UNITING -----------------------------------------------------------------

# This does exactly the opposite as separate.
# The first argument is the name of the new column. 
# The second and third argument are the names of the columns to be united.
# Use sep = "" to omit any separator to appear in the new column.

unite(NewVar, OldVar1, OldVar2, sep = "")



# VARIABLE NAMES ----------------------------------------------------------


# Can be used to rename variables to more convenient versions.

library(janitor)

data %>% 
  clean_names("snake", "lower_camel", "upper_camel", "all_caps")



# Use select to rename variables.
# This function will select every variable ending with "end", and renames it with
# new_name1, new_name2, and so on. Other attributes can be used as well.

data %>% 
  select(new_name = ends_with("end"), everything())



# DATES -------------------------------------------------------------------

# Can be used to parse dates of any given format.
# Indicate the function in the same order as the data to be used is.
# Works well with mutate.

library(lubridate)

data %>% 
  mutate(Variable1 = dmy(Variable1),
         Variable2 = ymd(Variable2),
         Variable3 = interval(Variable1, Variable2))






# STRINGS -----------------------------------------------------------------

# Converts a string to upper case.

str_to_upper("string")


# Converts a string to lower case.

str_to_lower("STRING")


# Searches for the presence or absence of a string.
# Returns logical values. Works well with mutate.

data %>% 
  mutate(newVariable = str_detect(Variable1, "string"))


# Works like str_detect, only that values are additionally replaced
# with the string in the third argument.

data %>% 
  mutate(newVariable = str_detect(Variable1, "string", "replacement"))


# Works like str_detect, only that values are removed if the condition 
# is true.

data %>% 
  mutate(newVariable = str_remove(Variable1, "string"))


# Removes whitespaces at the end or the beginning of a string.

data %>% 
  mutate(newVariable = str_remove(Variable1, "string"))





###########
# ANALYSIS
###########

# FILTER ------------------------------------------------------------------

# Can be applied to dataframes to search for rows containing certain patterns.
# Use & to search for different patterns together. Use | or %in% to search for either condition.


data %>% 
  
  filter(variable == "name" & variable > number)
  
  filter(variable == "name1" | variable = "name2")
  
  <=>
  
  filter(variable %in% c("name1", "name2"))

  

# SELECT ------------------------------------------------------------------

# Can be used to select specific variables from a dataframe.
# Everything puts everyother varibale behind the defined ones.

data %>% 
  select(variable1, variable2, 
         everything(), 
         ends_with("End"), 
         starts_with("start"), 
         contains("middle")
  )
  
  
# ARRANGE -----------------------------------------------------------------

# Can be used to sort a dataframe based on a single character or numeric variable.
# Use the default function to sort in ascending order, or include desc() for descending order.
  
data %>%

    arrange(variable)
  
    arrange(desc(variable))

    
# MUTATE ------------------------------------------------------------------

# Can be used to create new variables as a function of existing ones.
# Use existing variable names to overwrite them, or new ones to create new variables.
# The deafault version returns the whole dataframe, while transmute() only returns the new variable.

data %>% 
  
    mutate(NewVariable = Variable1 * Variable2)
    
    mutate(Variable1 = Variable1 * 2)
    
    transmute(Variable1 = Variable1 * 2)
    
    

# SUMMARIZE & GROUPING ----------------------------------------------------

# Summarize can be used to create summary statistics of a dataframe.
# The left part of the function can be used to indicate a name, while the right specifies the summary function and the variable to be analyzed.
    
data %>% 
    
    summarize(Var1_mean = mean(Variable1))
    
    summarize(Var1_median = median(Variable1))
    
# Group_by can be used before using summarize to specify a pattern which should be considered before calculating the summary statistic.
# When more than one variable is indicated, the statistic will be applied in the indicated order.
    
    
data %>% 
  
  group_by(variable1, variable2)
