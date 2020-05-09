#Install Packages Needed
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

#Read input from the user. Store inputed file name into the filename variable.
filename <- readline(prompt = "Please enter the file name with appropriate extension (Revised All Data.xlsx): ")

#read and store sheet 1 (group 1) of excel file
dataSheet1 <- read_excel(filename, sheet = "Group 1")
#Remove unessecary columns so that only the accession and animal ID's are present
#if you wanted to keep all columns, remove the followig line that appears after each dataframe read
dataSheet1 <- dataSheet1[-c(2:7)]

#read and store sheet 2 (group 2) of excel file
dataSheet2 <- read_excel(filename, sheet = "Group 2")
#Remove unessecary columns
dataSheet2 <- dataSheet2[-c(2:7)]

#read and store sheet 3 (group 3) of excel file
dataSheet3 <- read_excel(filename, sheet = "Group 3")
#Remove unessecary columns
dataSheet3 <- dataSheet3[-c(2:7)]

#read and store sheet 4 (group 4) of excel file
dataSheet4 <- read_excel(filename, sheet = "Group 4")
#Remove unessecary columns
dataSheet4 <- dataSheet4[-c(2:7)]

#read and store sheet 5 (group 5) of excel file
dataSheet5 <- read_excel(filename, sheet = "Group 5")
#Remove unessecary columns
dataSheet5 <- dataSheet5[-c(2:7)]

#read and store sheet 6 (group 6) of excel file
dataSheet6 <- read_excel(filename, sheet = "Group 6")
#Remove unessecary columns
dataSheet6 <- dataSheet6[-c(2:7)]

#read and store sheet 7 (group 7) of excel file
dataSheet7 <- read_excel(filename, sheet = "Group 7")
#Remove unessecary columns
dataSheet7 <- dataSheet7[-c(2:7)]

#read and store sheet 8 (group 8) of excel file
dataSheet8 <- read_excel(filename, sheet = "Group 8")
#Remove unessecary columns
dataSheet8 <- dataSheet8[-c(2:7)]

#read and store sheet 9 (group 9) of excel file
dataSheet9 <- read_excel(filename, sheet = "Group 9")
#Remove unessecary columns
dataSheet9 <- dataSheet9[-c(2:7)]

#read and store sheet 10 (group 10) of excel file
dataSheet10 <- read_excel(filename, sheet = "Group 10")
#Remove unessecary columns
dataSheet10 <- dataSheet10[-c(2:7)]

#read and store sheet 11 (group 11) of excel file
dataSheet11 <- read_excel(filename, sheet = "Group 11")
#Remove unessecary columns
dataSheet11 <- dataSheet11[-c(2:7)]

#read and store sheet 12 (group 12) of excel file
dataSheet12 <- read_excel(filename, sheet = "Group 12")
#Remove unessecary columns
dataSheet12 <- dataSheet12[-c(2:7)]

#Merge all the data sheets together and order them by their Accession key
#Use full_join in order to include all rows and coulumns, including ones that don't match
#Unmatched columns will input NA as a default. store in total_merge dataframe
merged_data_sheets <- full_join(dataSheet1, dataSheet2, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet3, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet4, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet5, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet6, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet7, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet8, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet9, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet10, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet11, by = "Accession")
merged_data_sheets <- full_join(merged_data_sheets, dataSheet12, by = "Accession")

#remove any rows with NA values and store in a new dataframe
final_merge <- na.omit(merged_data_sheets)

#transpose the data frame so that animal ID's are the row names and proteins are the column names
final_merge2 <- as.data.frame(t(final_merge))

#rename the column headers after the accession protein ID's located in row 1
#after that remove row 1 so the column headers are not listed twice
colnames(final_merge2) = as.character(unlist(final_merge2[1, ]))
final_merge2 = final_merge2[-1, ]


#Use the row_names_to_column function to move the row labels of the dataframe into it's own column
#We can now use this new Animal_ID column for comparisons in the following mutate function
final_merge2 <- rownames_to_column(final_merge2, var = "Animal_ID")

#Use mutate to add a new column named Test_Groups. Use the case_when function to label each Animal_ID
#when it matches any of the animal ID's (seperated by group) listed in the code below.
final_mutate_groups <- final_merge2 %>%
  mutate(Test_Groups = case_when(
    #Adding labels in new column for control group day 90 for animals matching following ID's
    Animal_ID == "38F2" | Animal_ID == "38F1" | Animal_ID == "119F1" | Animal_ID == "119F2" |
      Animal_ID == "124F1" | Animal_ID == "87F1" | Animal_ID == "87F2" | Animal_ID == "184F1" 
        ~ "Control_D90",
    
    #Adding labels in new column for overfed group day 90 for animals matching following ID's
    Animal_ID == "120F1" | Animal_ID == "120F2" | Animal_ID == "26F1" | Animal_ID == "26F2" | 
      Animal_ID == "117F1" | Animal_ID == "117F2" | Animal_ID == "109F1" | Animal_ID == "12F1"
        ~ "OverFed_D90",
    
    #Adding labels in new column for res group day 90 for animals matching following ID's
    Animal_ID == "19F1" | Animal_ID == "19F2" | Animal_ID == "20F1" | Animal_ID == "20F2" |
      Animal_ID == "43F1" | Animal_ID == "43F2" | Animal_ID == "39F1" | Animal_ID == "127F3"
        ~"Restricted_D90",
    
    #Adding labels in new column for control group day 135 for animals matching following ID's
    Animal_ID == "41F1" | Animal_ID == "79F1" | Animal_ID == "79F2" | Animal_ID == "81F1" |
      Animal_ID == "81F2" | Animal_ID == "75F2" | Animal_ID == "75F3" | Animal_ID == "107F1" |
      Animal_ID == "107F2" ~ "Control_D135",
    
    #Adding labels in new column for res group day 135 for animals matching following ID's
    Animal_ID == "94F1" | Animal_ID == "94F2" | Animal_ID == "101F1" | Animal_ID == "118F1" |
      Animal_ID == "118F2" | Animal_ID == "3F2" | Animal_ID == "3F3" | Animal_ID == "69F1" |
      Animal_ID == "69F2" ~"Restricted_D135",
    
    #Adding labels in new column for overfed group day 135 for animals matching following ID's
    Animal_ID == "63F1" | Animal_ID == "63F2" | Animal_ID == "89F1" | Animal_ID == "89F2" | 
      Animal_ID == "93F1" | Animal_ID == "93F2" | Animal_ID == "56F1" | Animal_ID == "56F2" |
      Animal_ID == "21F1" ~ "OverFed_D135",
    
    #Add NA to new column for any unmatched animal ID
    TRUE ~ "NA"
  ))

#move the Test_Group column from the last column to the first column so that it appears near the Animal ID's
arranged_final_mutate_groups <- final_mutate_groups %>%
  select(Test_Groups, everything()) %>%
  arrange(Test_Groups)


#Filter out the test groups into their own data sets for analysis

#Filter out all day 90 animals in all test groups into their own data frame.
#Arrage them so the control, over, and res groups all appear together on the data frame.
day90_group <- arranged_final_mutate_groups %>%
  filter(str_detect(Test_Groups, "D90")) %>%
  arrange(Test_Groups)

#Filter out all day 135 animals in all test groups into their own data frame
#Arrage them so the control, over, and res groups all appear together on the data frame.
day135_group <- arranged_final_mutate_groups %>%
  filter(str_detect(Test_Groups, "D135")) %>%
  arrange(Test_Groups)

#vector to store all column numbers that contain numbers as factors
columns_to_convert <- c(3:986)

#Use the apply function to apply the as.numeric function to every column containing numbers as factors (using the vector created above)
#These next two functions will convert all factors in the data frame to the numerical class for plotting purposes.
day90_group[ ,columns_to_convert] <- apply(day90_group[ , columns_to_convert], 2,
                                           function(x) as.numeric(as.character(x)))

day135_group[ ,columns_to_convert] <- apply(day135_group[ , columns_to_convert], 2,
                                           function(x) as.numeric(as.character(x)))

#Use the ggplot2 package to create 2 box plot graphs for both the day 90 and day 135 timepoint data frames that showcase
#the three treatment groups.These can then be analyzed side by side.

#Box plot comparing the protein expression between the three day 90 treatment groups. The protein to be plotted is
#entered in by the user. (For example: A2SW69)
protein <- readline(prompt = "Please enter any protein from the dataframe (Ex = A2SW69 or W5QJ50 or W5NTG3): ")

#Box plot that shows the Relative Abundance of the user inputted protein for the 3 test groups sampled at day 90
day90_boxplot <- ggplot(day90_group, aes_string(x = "Test_Groups", y = protein, group = "Test_Groups", fill = "Test_Groups")) +
  geom_boxplot() + 
  ggtitle(paste("Day 90 Samples: Relative Protein Abundance for Protein ", protein)) +
  scale_y_continuous(name = "Relative Protein Abundance", breaks = seq(0.0, 300, 5.0)) +
  scale_x_discrete(name = "Test Groups", breaks = c("Control_D90", "OverFed_D90", "Restricted_D90"),
                   labels = c("Control Diet", "Over Fed Diet", "Restricted Diet"))+
  scale_fill_discrete(name = "Test Groups", labels = c("Control", "Over Fed Diet", "Restricted Diet"))

#View the day 90 box plot
day90_boxplot

#Box plot that shows the Relative Abundance of the user inputted protein for the 3 test groups sampled at day 135
day135_boxplot <- ggplot(day135_group, aes_string(x = "Test_Groups", y = protein, group = "Test_Groups", fill = "Test_Groups")) +
  geom_boxplot() + 
  ggtitle(paste("Day 135 Samples: Relative Protein Abundance for Protein ", protein)) +
  scale_y_continuous(name = "Relative Protein Abundance", breaks = seq(0.0, 300, 5.0)) +
  scale_x_discrete(name = "Test Groups", breaks = c("Control_D135", "OverFed_D135", "Restricted_D135"), labels = c("Control Diet", "Over Fed Diet", "Restricted Diet"))+
  scale_fill_discrete(name = "Test Groups", labels = c("Control", "Over Fed Diet", "Restricted Diet"))

#View the day 135 box plot
day135_boxplot













