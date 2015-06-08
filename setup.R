



## Kevin Sparks 6/8/15

# Instruction
# 1. Create a folder with the name of the experiment;
# 2. In the experiment folder, create a folder named "zip" and put all participant zip files into the "zip" folder;
# 3. Load the packages seen below
# 4. Define a path variable as a string to the location of the experiment folder (there should be a sub-folder "zip")
# 5. Run CatDirectorySetup


#install.packages("gplots")
require(gplots)
#install.packages("vegan")
require(vegan)
#install.packages("clusteval")
require(clusteval)
require(grid)

# Path to where the experiment folder is
path <- "/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests"

# Creates the necessary sub folders in experiment folder
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


