## Kevin Sparks 3/4/15
## Functions taken from "catanalysis.R" and "catAnalysisExtension.R" from "jinlong25_catanalysis" repo (cross-referenced with "catanalysis2501" (geo terms))

## Notes
# There seemed to be two sets of functions being defined among various catanalysis scripts. The first, denoted by 6 lines of "#"s and called "Function list Pt. 1" 
# (followed with a set of executables running those functions denoted by three lines of "#"s), and the second, denoted by 6 lines of "#"s and called "Functions list Pt. 2"

# "clusterval" package not available in R 3.0.0 









# Instruction
# 1. Create a folder with the name of the experiment;
# 2. In the experiment folder, create a folder named "zip" and put all participant zip files into the "zip" folder;
# 3. Run the entire script ("function_list_pre.R"), then run "function_list_1.R", followed by "function_list_2.R";
# 4. To create dendrograms at different solutions, manually change the number in the last line of the script.
# 5. Go find the result in the experiment folder

# KlipArt Instruction
# A klipart folder will be created inside the experiment folder. In the klipart folder:
# 1. Zip the matrices folder into a zip file;
# 2. Put a icons.zip file that contains all the icon files.


#install.packages("gplots")
require(gplots)
#install.packages("vegan")
require(vegan)
#install.packages("clusteval")
require(clusteval)
require(grid)

# Clear the workspace
rm(list=ls())

# Begin user input #
# Path to where the experiment folder is
path <- "C:/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests"

# Checks if "/" exists after path. If not, one is added
if(substr(path, nchar(path), nchar(path)) != "/") {
	path <- paste(path, "/", sep = "")
}

setwd(path)

# Define the name of the experiment
scenario.name <- "2501 geotermsN"

# # Define the max number of clusters
# max.cluster <- 9

# End user input #




##########
##########
##########




CatDirectorySetup <- function(path) {
	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	# Auto-create two subfolders "ism" and "matrices"
	dir.create(paste(path, "ism/", sep=""))
	klipart.path <- paste(path, scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path)
	dir.create(paste(klipart.path, "matrices/", sep="")) 
	dir.create(paste(path, "matrices/", sep="")) 
}

CatDirectorySetup(path)




# Participant counter: count the number of participants
ParticipantCounter <- function(path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	
	#Construct the zip folder path and list all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	#Get the total number of participants (zip files)
	np <- length(files)
	
	#Return the total number of participants as an integer
	return(np)
}

number.of.participants <- ParticipantCounter(path)




#Icon counter: count the number of icons(items) used in the experiment
IconCounter <- function(path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}

	#Construct the zip folder path and list all the zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)

	#Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	#Unzip the file
	first.p <- unzip(paste(zip.path, files[1], sep = ""))
	
	#Construct the full file name for icons.csv
	icons.csv <- paste("./", first.p.number, "/", substring(files[1],1,8), "icons.csv", sep = "")
	
	#Read in icons.csv
	icons <- read.csv(icons.csv, header = F)
	
	#Get the number of icons used in the experiment
	n.icons <- nrow(icons)
	
	#Return the number of icons
	return(n.icons)
}

number.of.icons <- IconCounter(path)




# Icon list getter: get a list of icon names
# It also saves the icon.csv needed for KlipArt
IconListGetter <- function(path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	
	# Construct the zip folder path and list all the zip files 
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Unzip the zip file from the 1st participant
	first.p <- unzip(paste(zip.path, files[1], sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for icons.csv
	icons.csv <- paste("./", first.p.number, "/", substring(files[1],1,8), "icons.csv", sep = "")
	
	# Read in icons.csv
	icons <- read.csv(icons.csv, header = F, stringsAsFactors = F)
	
	# Reorder icon names by icon index
	icons <- icons[order(icons[, 1]), ]
	
	# Extract the icon names from the table (excluding the path name and file extensions)
	icon.list <- icons[, 2]
	for(i in 1:length(icon.list)) {
		end <- regexpr("\\.[^\\.]*$", icon.list[i])[1]
		icon.list[i] <- substr(icon.list[i], 9, end - 1)
	}
	
	# Extract the icon names with file type (e.g. .jpg) for KlipArt
	icon.list.klipart <- icons
	for(j in 1:nrow(icon.list.klipart)) {
		icon.list.klipart[j, 2] <- substr(icon.list.klipart[j, 2], 9, nchar(icon.list.klipart[j, 2]))
	}
	colnames(icon.list.klipart) <- c("index", "icon_names")
	
	# Sort the icon list by index
	icon.list.klipart <- icon.list.klipart[order(icon.list.klipart$index), ]
	
	# Export the list as a csv file
	write.table(icon.list.klipart, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "icon.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
	
	#Return the icon list as a vector
	return(sort(icon.list))
}

icon.list <- IconListGetter(path)




