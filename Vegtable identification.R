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
  labels <- basename(path_dir(filepaths)) #foldernames are labels
  
  df <- tibble( #creates a table 
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
print(unique(test_df$Label)) #prints my list
head(test_df, 5) #shows only 5 rows
#works nicely! 

#(Step 3!!! Create a DataFrame with one Label of each category) lowkey we could cut a lot of this section out and be fine
#df_unique = train_df.copy().drop_duplicates(subset=["Label"]).reset_index()
#essentially this would make a copy and drop duplicates 

#keep one image per label
unique_df <- test_df %>% 
# distinct() removes duplicate rows based on the Label column and keep_all = TRUE' keeps all columns in the output
  distinct(Label, .keep_all = TRUE)

#(Step 4!!! Display some pictures of the dataset)
#they want to create a 6*6 column with picture size (8,7)<--Magick can help with that...maybe

# For each image, read it and add the label at the bottom
imgs <- list()
for (i in 1:nrow(unique_df)) {
  img <- image_read(unique_df$Filepath[i])
  img <- image_annotate(img, unique_df$Label[i], size = 20, gravity = "south",
                        color = "white", boxcolor = "black")
  imgs[[i]] <- img
}

# put the images in rows of 6 and stack the rows on top of each other
rows <- list()
for (i in seq(1, length(imgs), by = 6)) {
  
  # Get 6 images 
  row_imgs <- imgs[i:min(i + 5, length(imgs))]
  
  # Convert list of images to magick images 
  row_vec <- do.call(c, row_imgs)
  
  # side by side images
  rows[[length(rows) + 1]] <- image_append(row_vec)
}

# Stack rows vertically
final_grid <- image_append(do.call(c, rows), stack = TRUE)

print(final_grid)

#Kritik Seth, "Fruits and Vegetables Image Recognition Dataset," Kaggle 2020 [https://www.kaggle.com/kritikseth/fruit-and-vegetable-image-recognition]

