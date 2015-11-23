library(dplyr)

data_dir <- "UCI HAR Dataset"


load.dataset <- function(subjects_path, labels_path, data_path, col_names, activity_names) {

  data <- cbind(
    read.table(subjects_path),
    read.table(labels_path),
    read.table(data_path)                    
  )
  
  # set column names
  colnames(data) <- col_names
  
  # add activity labels
  data$activityname <- factor(data$activityname, labels = activity_names)
  
  # select mean and std dev features
  select(data, subjectid, activityname, matches("mean\\(|std\\("))
}
load.colnames <- function() {
  feature_names_path <- file.path(data_dir, "features.txt")
  names_table <- read.table(feature_names_path, row.names=1, stringsAsFactors = FALSE)
  make.unique(c("subjectid", "activityname", names_table$V2))
}
load.activitynames <- function() {
  activity_names_path <- file.path(data_dir, "activity_labels.txt")
  read.table(activity_names_path, stringsAsFactors = FALSE)$V2    
}
expand.colnames <- function(col_names) {
  replacements <- list(
    c("BodyBody", "Body"), 
    c("^t", "time"), 
    c("^f", "frequency"),
    c("-mean\\(\\)", "Mean"),
    c("-std\\(\\)", "Standarddeviation"),
    c("Gyro", "Gyroscope"),
    c("Acc", "Acceleration"),
    c("Mag", "Magnitude"),
    c("-(X|Y|Z)", "\\1")
  )
  for (replacement in replacements) {
    col_names <- sub(pattern = replacement[1], replacement = replacement[2], x = col_names)
  }
  col_names
}





# load column names
colnames <- load.colnames()
activitynames <- load.activitynames()
data.test <- load.dataset(
  file.path(data_dir, "test", "subject_test.txt"),
  file.path(data_dir, "test", "Y_test.txt"),
  file.path(data_dir, "test", "X_test.txt"),
  colnames,
  activitynames
)
data.train <- load.dataset(
  file.path(data_dir, "train", "subject_train.txt"),    
  file.path(data_dir, "train", "Y_train.txt"),
  file.path(data_dir, "train", "X_train.txt"),
  colnames,
  activitynames
)

data.all <- rbind_list(data.train, data.test)

colnames(data.all) <- expand.colnames(colnames(data.all))
means <- data.all %>% 
  group_by(subjectid, activityname) %>% 
  summarise_each(funs(mean), matches("mean|standarddeviation"))

write.table(means, file="tidy_data.txt",  row.name=FALSE)