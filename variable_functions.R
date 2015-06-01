## Kevin Sparks 6/1/15

# Notes
# "clusterval" package not available in R 3.0.0 

# Instruction
# 1. Create a folder with the name of the experiment;
# 2. In the experiment folder, create a folder named "zip" and put all participant zip files into the "zip" folder;
# 3. Load the packages seen below
# 4. Define a path variable as a string to the location of the experiment folder
# 5. Run CatDirectorySetup


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

# Creates the necessary sub directories in experiment folder
# Parameters
# path: string, path to experiment directory
# scenario.name: string, name of the experiment
CatDirectorySetup <- function(path, scenario.name) {
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

scenario.name <- "scenario_name_here"
CatDirectorySetup(path, scenario.name)




# End user input #
##########
##########
##########




# Removes all the unzipped participant folders in working directory
CatCleanUp <- function(path) {
	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
  	zip.path <- paste(path, "zip/", sep = "")
  	zip.files <- list.files(zip.path)
  	folder.names <- substr(zip.files, 1, nchar(zip.files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}
}

CatCleanUp(path)




# Participant counter: count the number of participants
# Parameters
# path: string, path to experiment directory
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
# Parameters
# path: string, path to experiment directory
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

	unlink(substring(paste(getwd(), "/", files[1], sep = ""), 1, nchar(paste(getwd(), "/", files[1], sep = "")) - 4), recursive = TRUE)
	
	#Return the number of icons
	return(n.icons)
}

number.of.icons <- IconCounter(path)




# Icon list getter: get a list of icon names
# It also saves the icon.csv needed for KlipArt
# Parameters
# path: string, path to experiment directory
# scenario.name: string, name of the experiment
IconListGetter <- function(path, scenario.name) {

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
	write.table(icon.list.klipart, file = paste(path, scenario.name, "-klipart/", "icon.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)

	unlink(substring(paste(getwd(), "/", files[1], sep = ""), 1, nchar(paste(getwd(), "/", files[1], sep = "")) - 4), recursive = TRUE)
	
	#Return the icon list as a vector
	return(sort(icon.list))
}

icon.list <- IconListGetter(path, scenario.name)




