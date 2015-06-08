# These "analysis functions" mostly take data returned from the "read functions" and run some sort of analysis with them.




# OsmGenerator: Adds all isms (created by ReadIsms) to one osm matrix.
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




# MdsScaling: performs multi-dimensional scaling on an overall similarity matrix (created by OsmGenerator). 
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator by adding all isms together
MdsScaling <- function(osm) {
	osm.dist <- dist(osm, method = "euclidean")
	mds <- cmdscale(osm.dist)
	return(mds)
}

mds <- MdsScaling(Osm)




# ClusterAnalysis: performs cluster analysis on a given overall similarity matrix and returns a list, where the 1st entry is a dendrogram, and the 2nd entry is a cophenectic matrix
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator by adding all isms together
# number.of.participants: integer, the number of participants in the experiment can be created by ParticipantCounter
# cluster.method: string, optional parameter where the default is set to Ward's method (user has choice between options provided by the hclust function)
ClusterAnalysis  <- function(osm, number.of.participants, cluster.method="ward.D") {
	
	# Participants minus osm generates dissimilarity
	clust <- hclust(method = cluster.method, as.dist(number.of.participants - osm))
	
	# compute and save cophenectic matrices
	coph <- as.matrix(cophenetic(clust)) 
	
	# Export the dendrograms as a jpeg files
	dend.clust <- as.dendrogram(clust)

	cluster <- list()
	cluster[[1]] <- dend.clust
	cluster[[2]] <- coph

	return(cluster)

}

cluster.output <- ClusterAnalysis(Osm, number.of.participants)




# ParticipantSimilarity: Participant similarity analysis
# Parameters
# isms: list of matrices, a list of participants' isms
# ism.list: character vector, list of all the isms you want included (if you want to include all of them, list.files(ism.path) will work)
ParticipantSimilarity <- function(isms, ism.list) {
	
	np <- length(isms)

	# Calculate participant similarity matrices of all pairs of partcipants based on the hamming distance of their ISMs (dm) and Jaccard index (dm_jaccard)
	dm <- matrix(0, ncol = np, nrow = np)
	dm.jac <- matrix(0, ncol = np, nrow = np)
	dm.rand <- matrix(0, ncol = np, nrow = np)
	for (i in 1:np) {
		for (j in 1:np) {
			dm[i, j] <- sum(abs(isms[[i]] - isms[[j]]))
			
			m11 <- sum(isms[[i]] * isms[[j]])
			m01 <- sum(abs(1-isms[[i]]) * isms[[j]])
			m10 <- sum(isms[[i]] * abs(1-isms[[j]]))
			m00 <- sum(abs(1-isms[[i]]) * abs(1-isms[[j]]))
			
			dm.jac[i, j] <- m11 / (m01+m10+m11) 
			
			dm.rand[i, j] <- (m11 + m00) / (m01+m10+m00+m11)
		}
	}
	
	# Extract the participant number of all participants and store them in a vector named names
	names <- c()
	for (i in 1:length(ism.list)) {
		name <- ism.list[i]
		names <- append(names, substr(name, 12, nchar(name) - 5))
	}
	
	# Assign participants numbers as the row&column names of the participant similarity matrix (dm)
	colnames(dm) <- names
	rownames(dm) <- names
	colnames(dm.jac) <- names
	rownames(dm.jac) <- names
	colnames(dm.rand) <- names
	rownames(dm.rand) <- names

	participant.similarity.output <- list()
	participant.similarity.output[[1]] <- dm
	participant.similarity.output[[2]] <- dm.jac
	participant.similarity.output[[3]] <- dm.rand

	return(participant.similarity.output)
	
}

ism.path <- paste(path, "/ism", sep="")
participant.similarity.output <- ParticipantSimilarity(isms, list.files(ism.path))







