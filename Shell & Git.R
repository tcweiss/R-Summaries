

###################################
#
# THIS SCRIPT INCLUDES USEFUL COMMANDS 
# FOR UNIX SHELL AND GIT
#
####################################





# SHELL -------------------------------------------------------------------


# Shows the current working directory

pwd


# Lists all files within a given directory.

ls


# Absolute paths have the same value, no matter in which directory we are.
# Relative paths specify a location starting from where we are.
# If we are currenty in /home/repl, the following leads to the same result:

ls seasonal

ls /home/repl/seasonal


# Can be used to change to another directory.
# Use .. to move one level up, or ~ to move to the home directory.

cd

cd .. 

cd ~

  
# Makes a copy of the first file with the name and directory of the second file.
# If the last parameter is an existing directory, all files are copied into that directory.
  
cp seasonal/original.csv dental/copy.csv

cp seasonal/original.csv seasonal/copy.csv dental


# Moves the indicated files to the directory indicated in the last parameter.
# If no directory is indicated as a last parameter, the first file is "moved" into the second. This can be used to rename files.

mv seasonal/original.csv seasonal/copy.csv dental

mv old-name.csv new-name.csv


# Can be used to delete files.

rm file.txt


# Can be used to look at the content of a file-
# Type q to exit the file again.

less file.txt


# Can be used to delete directories, if they are emtpy.

rmdir seasonal


# Can be used to create directories.

mkdir dental



# SHELL ARGUMENTS ---------------------------------------------------------

# With rm, all files below are removed as well. 
# With ls, the order of files displayed is reversed.

rm -r
ls -r


# Shows all files, even the hidden ones.

ls -a

# Shows more information about a file ("long").

ls -l

# Shows files in chronological order.

ls -t



# SHELL WILDCARDS ---------------------------------------------------------

# Any number of any combination of characters.
# The below lists/removes any files with .html ending.

ls *.html
rm *.html


# Any single character-
# The below targets all files with three characters and .html ending.

ls ???.html
rm ???.html

# GIT ---------------------------------------------------------------------

# Can be used to check the changes since the last time sth was saved.

git status


# Can be used to explicitly check the changes within a file or directory since the last staging.
# Add -r HEAD to explicitly check the changes within a file or directory since the last commit.


git diff

git diff text.csv


git diff -r HEAD 

git diff -r HEAD text.csv


# Can be used to edit a file.
# Use Ctrl + O to save changes, and Ctrl + X to exit.

nano text.csv


# Can be used to stage a file in which changes have been made.

git add text.csv


# Can be used to commit all changes that have been saved.
# Add -m "Message" to add a log message. Use --amend to change a message.

git commit -m "Add a reference."

git commit --amend -m "Changing a reference."


# Can be used to check the log message of a file or directory.

git log

git log text.csv








