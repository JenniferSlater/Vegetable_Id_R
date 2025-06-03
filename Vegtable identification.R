library(magick) # helps look at and edit images if we need to check stuff
library(tidyverse) # helps us work the data better
library(fs)# lets me list directories

#I downloaded the vegetable training for this
#Test: Contains 10 images per category.


# create list with file paths for training (STEP 1!!! LOADING AND PREPROCESSING)
test<-"test"
test_filepaths <- dir_ls(test, recurse = TRUE, glob = "*.jpg") #Gets list of all.jpg files

# "Create a DataFrame with the filepath and the labels of the pictures"
proc_img <- function(filepaths) {
  labels <- basename(path_dir(filepaths))
  
  df <- tibble(
    Filepath = as.character(filepaths),
    Label = labels
  ) %>%
    slice_sample(prop = 1) # Shuffle rows
  
  return(df)
}

# Process the test data
test_df <- proc_img(test_filepaths)

# View the first few rows
print(test_df)


# (STEP 2!!! PRINTING DATA SET) EZ!!
print('-- Training set --')
print(paste("Number of pictures", nrow(test_df)))
print(paste("Number of different labels:", length(unique(test_df$Label))))
print(unique(test_df$Label))
head(test_df, 5)#works nicely!


#(Step 3!!! Create a DataFrame with one Label of each category)
    #df_unique = train_df.copy().drop_duplicates(subset=["Label"]).reset_index()
    #essentially this would make a copy and drop duplicates
unique_df <- test_df %>% 
# distinct() removes duplicate rows based on the Label column and keep_all = TRUE' keeps all columns in the output
  distinct(Label, .keep_all = TRUE)

# Display some pictures of the dataset
#they want to create a 6*6 column with picture size (8,7)<--Magick can help with that...maybe


#Kritik Seth, "Fruits and Vegetables Image Recognition Dataset," Kaggle 2020 [https://www.kaggle.com/kritikseth/fruit-and-vegetable-image-recognition]

