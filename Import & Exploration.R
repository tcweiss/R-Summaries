

###################################
#
# THIS SCRIPT INCLUDES USEFUL FUNCTIONS 
# FOR IMPORTING AND EXPLORING DATA
#
####################################



# IMPORTING DATA ----------------------------------------------------------

# Loads csv files.
# Skip indicates rows to be skipped, na indicates values to be replaced with NA.

read.csv("filename.csv", skip = 3, na = c("", "NA"))




# Changes the types of a dataframe's columns.


read_xyz("file.txt", 
         
         colt_types = cols(
           
          column1 = col_date(format = "%d %B %Y"),
          column2 = col_number(),
          column3 = col_factor(levels = NULL)
         )
        )





# EXPLORING DATA ----------------------------------------------------------

# Shows all variables of a dataframe.

glimpse()


# Shows a summary on all variables, depending in their class.

skim()


# Calculates some basic summary statistics. Works well with skim.

summary()


# Counts the number of observations for the indicated variables.
# If more than one variable is indicated, the function works like group_by.
# Piping the result into one variable less "rolls" up the resulting df by one level.

count(variable1, variable2)

count(variable1, variable2) %>% 
  count(variable1)


# Shows an overview of all distinct observations within a variable.

distinct(variable1)








