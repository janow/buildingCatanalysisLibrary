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
path <- "/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests"

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




# Returns a list of participant isms
# Parameters
# ism.path: string, full path to the ism directory 
# ism.list: character vector, list of all the isms you want included (if you want to include all of them, list.files(ism.path) will work)
# number.of.icons: integer, the total number of icons in the experiment created by IconCounter
ReadIsms <- function(ism.path, ism.list, number.of.icons) {

  # Checks if "/" exists after ism.path. If not, one is added
  if(substr(ism.path, nchar(ism.path), nchar(ism.path)) != "/") {
    ism.path <- paste(ism.path, "/", sep = "")
  }

  # creates an empty list to store imported isms
  isms <- list()
    
  for (i in 1:length(ism.list)) {

    # gathers the current ism file name from the ism.list parameter in the for loop
    matrix.i.name <- ism.list[i]

    # reads in the current ism file
    matrix.i <- read.delim(paste(ism.path, matrix.i.name, sep=""), header = F, sep = " ", stringsAsFactors = F)

    # converts the ism file to a matrix
    matrix.i <- data.matrix(matrix.i[1:number.of.icons, ])
        
    # adds that ism matrix to the isms list created at the begining of the function
    isms[[i]] <- matrix.i
  }

  # returns a list of the isms
  return(isms)

}

ism.path <- paste(path, "/ism", sep="")
isms <- ReadIsms(ism.path, list.files(ism.path), number.of.icons)




# Adds all isms (created by ReadIsms) to one osm matrix
# Parameters
# isms: list of matrices, a list of participants' isms
# icon.list: character vector, a list of the names of the icons created by IconListGetter
OsmGenerator <- function(isms, icon.list) {

  # creates an empty osm matrix
  osm <- matrix(0, nrow(isms[[1]]), ncol(isms[[1]]))
    
  # loops through all the isms in the isms parameter input
  for (ism in isms) {
    # adds each ism (in effect adding all the isms together)
    osm <- osm + ism
  }

  # defining the row and column names for the osm from the icon.list parameter
  dimnames(osm) <- list(icon.list,icon.list)

  # returns the osm matrix
  return(osm)
}

Osm <- OsmGenerator(isms, icon.list)










