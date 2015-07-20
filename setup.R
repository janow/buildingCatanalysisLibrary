



## Kevin Sparks 7/20/15

# Instruction
# 1. Create a folder with the name of the experiment;
# 2. In the experiment folder, create a folder named "zip" and put all participant zip files into the "zip" folder;
# 3. Load the packages seen below
# 4. Define a path variable as a string to the location of the experiment folder (there should be a sub-folder "zip")

#install.packages("gplots")
require(gplots)
#install.packages("vegan")
require(vegan)
#install.packages("clusteval")
require(clusteval)
require(grid)

# Setting the basic path and scenario name variables, as well as setting the working directory to the path (this isn't necessary, just convenient).
path <- "/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests"
setwd(path)
scenario.name <- "scenario_name_here"



