pkgs = c("SearchTrees","geosphere")
to_install = !pkgs %in% installed.packages()
if(sum(to_install) > 0){
  install.packages(pkgs[to_install])
}
lapply(X = pkgs, FUN = library, character.only = T)

source("R/all_England.R")


# Function to find the k nearest neighbours to each point in SpatialPoints spdf1 in SpatialPoints spdf2
findNearest = function(spdf1, spdf2, k){
  # https://stackoverflow.com/questions/37333747/for-each-point-distance-to-nearest-point-in-second-dataset-in-r
  tree = SearchTrees::createTree(coordinates(spdf1))
  index = SearchTrees::knnLookup(tree, newdat = coordinates(spdf2), k=k)
  return(index)
}

## Plot the first 50 schools (red) and their (k-1) nearest neighbours (k=1 is the reference school itself)
## (This is just to check it is working)
# k = 3
# nearest_indices = findNearest(schools, schools, k=k)
# schools_test_sample = sample(1:nrow(schools), 50) # Get random sample of schools
# plot(schools, pch=1, cex=0.2)
# for(i in schools_test_sample){ # Plot each school in sample (red) and its (k-1) nearest neighbours (green)
#   for(j in 1:k){
#     if(j ==1){
#       points(schools[index[i,j],], pch=16, col="red")
#     }else{
#       if(j <= length(index[i,])) # For some less than k nearest neighbours are found for some reason
#         points(schools[index[i,j],], pch=16, col="green")
#     }
#   }
# }


# Function to get the centroid of a set of points in a SpatialPointsDataFrame
getPointsCentroid = function(spdf){
  original_proj = proj4string(spdf)
  pnts = coordinates(spdf)
  new_pnts = cbind(coordinates(pnts), as.numeric(6378137)) #geosphere::distCosine pop-up shows this value for Earth's radius
  new_pnts = SpatialPoints(new_pnts)
  proj4string(new_pnts) = original_proj
  new_pnts = spTransform(new_pnts, CRS("+init=epsg:4978") ) # convert to geocentric Cartesian coordinates
  cent_x = mean(coordinates(new_pnts)[,1])
  cent_y = mean(coordinates(new_pnts)[,2])
  cent_z = mean(coordinates(new_pnts)[,3])
  cent = c(as.numeric(cent_x), as.numeric(cent_y), as.numeric(cent_z))
  cent = matrix(c(as.numeric(cent_x), as.numeric(cent_y), as.numeric(cent_z)), nrow=1, ncol=3)
  cent = SpatialPoints(cent, CRS("+init=epsg:4978") )
  cent = spTransform(cent, CRS(original_proj))
  return(cent)
}

# Check that the getPointsCentroid() function works
#plot(schools[1:100,])
#points(getPointsCentroid(schools[1:100,]), pch=16, col="red")

# Find the two nearest schools to each school (k=1 will be the base school itself)
nearest_indices = findNearest(schools, schools, k=3)

# Next we compute the actual distances to the nearest 2 schools just found 
# (done this way to avoid computing the full n_schools x n_schools distance matrix)

# Set the distance within which neighbouring schools' flows will be merged (in metres)
thresh = 1000

# Compute the distances to the two nearest schools and put it in a data.frame
dists_df = data.table::data.table(base=integer(nrow(schools)),
                                  nearest1=integer(nrow(schools)),
                                  distance1=double(nrow(schools)),
                                  nearest2=integer(nrow(schools)),
                                  distance2=double(nrow(schools)))
for(i in 1:nrow(schools)){
  dists_df[i, "base"] = i
  for( j in 2:length(nearest_indices[i,]) ){
    if(j==2){
      dists_df[i, "nearest1"] = nearest_indices[i,j]
      dists_df[i, "distance1"] = geosphere::distHaversine(schools[i,],  schools[nearest_indices[i,j],])
    }
    if(j==3){
      dists_df[i, "nearest2"] = nearest_indices[i,j]
      dists_df[i, "distance2"] = geosphere::distHaversine(schools[i,],  schools[nearest_indices[i,j],])
    }
  }
}

dists_df
#nrow(dists_df[dists_df$base != 0,])

dists_df[(dists_df$distance1 < thresh) | (dists_df$distance2 < thresh),]

# For each of the two nearest schools check if their distance is less than thresh,
#   and if so append them to a list of schools to be merged
nearestA = subset(dists_df, distance1 < thresh, select=c(base, nearest1))
names(nearestA)[names(nearestA)=="nearest1"] = "nearest"
nearestB = subset(dists_df, distance2 < thresh, select=c(base, nearest2))
names(nearestB)[names(nearestB)=="nearest2"] = "nearest"
nearest = rbind(nearestA, nearestB )

# Remove duplicate combinations of pairs of schools in the list of schools to be merged
#   (e.g. entry 13  15 is equivalent to 15  13)
# https://stackoverflow.com/questions/14078507/remove-duplicated-2-columns-permutations
min = pmin(nearest$base, nearest$nearest)
max = pmax(nearest$base, nearest$nearest)
perms = as.numeric(interaction(min, max))
nearest = nearest[match(unique(perms), perms),]
nearest = data.frame(nearest)
nearest = nearest[nearest$base != nearest$nearest, ]

#nearest = nearest[with(nearest, order(base)), ]
#num_nodes = length(unique(c(nearest$base, nearest$nearest)))

# There will be overlap between the pairs of nearest schools which are close enough to be merged
#  (e.g. 13  15, 15  80....)
# These pairs of schools therefore need to be joined into larger groups where they overlap
#  (failure to do so will result in problems where schools present in more than one pair get clustered
#   differently according to the order in which the pairs are clustered)

# To solve this we think of this problem as a graph, and look for the maximal cliques
#   (the largest sets of nodes in the graph which are all connected to each other)
# This is exactly what we need. We now create this graph using our pair list of schools to be clustered
#   as an edge list (which fully specifies nodes and edges) for an undirected graph

# This will give the largest sets of schools which are all traversable by travelling no more than thresh (1 km)

# https://stackoverflow.com/questions/29730624/how-to-split-an-igraph-into-connected-subgraphs
# http://kateto.net/networks-r-igraph
#graph1 = igraph::graph_from_edgelist(as.matrix(nearest, nrow = nrow(nearest), ncol=2), directed = FALSE)
graph1 = igraph::graph.data.frame(nearest, directed = FALSE)
graph1 = igraph::simplify(graph1) # Remove loops, and multiple edges between same nodes

plot(graph1)

# Find the maximal cliques
#cliques = igraph::cliques(graph1)
max_cliques = igraph::maximal.cliques(graph1, min = 2)
length(max_cliques)

# Get the clusters these cliques correspond to
#  (i.e. the cliques are just sets of nodes, their corresponding clusters include the edges as well, so 
#   are an actual graph)
clusters = igraph::clusters(graph1)
#clusters = clusters[clusters$csize >= 2]
clusters$no
clusters$membership
clusters$csize

# Print sparse matrix for graph
#graph1[]

# Decompose the full graph into the clusters for the maximal cliques found
dg = igraph::decompose.graph(graph1, min.vertices = 2)
length(dg)

# Check that these clusters correctly match entries in our schools pair list
#plot(dg[[1]])
#nearest[(nearest$base == 3),]
#nearest[(nearest$base == 11),]
#nearest[(nearest$base == 50),]
#nearest[(nearest$base == 1421),]

# Example of the largest cluster found
#plot(dg[[45]])

# Dump the indices for the schools to be clustered into an array
schools_clusters = array(dim = c(clusters$no, max(clusters$csize)))
for(i in 1:length(dg)){
  clust = c(as.numeric(igraph::vertex_attr(dg[[i]])$name))
  schools_clusters[i, 1:length(clust)] = clust
}

# We have been working with the schools table, but when merging the schools we will need
#   to merge their flows, which are in the flows ("s") table.
# Therefore get the corresponding school URNs to then merge these entries in the flows table

# However the destinations (schools) and the Origin-Destination (flows) tables get joined
#  inside the od2line() function, so we must merge schools and consistently assign them 
#  new URN numbers in both the schools and the flows tables

schools_clusters_URNs = array(dim = dim(schools_clusters))
for(i in 1:nrow(schools_clusters)){
  schools_idx = subset(schools_clusters[i,], !is.na(schools_clusters[i,]))
  schools_clusters_URNs[i, 1:length(schools_idx)] = schools[schools_idx, ]$URN
}
  
for(i in 1:nrow(schools_clusters_URNs)){
  sel = s$URN %in% schools_clusters_URNs[i,]
  temp_df = group_by(s[sel,]) 
}

#igraph::vertex_attr(graph1)
#igraph::edge_attr(graph1)
#igraph::graph_attr(graph1)

#agg_nearest = data.frame(aggregate(nearest ~ base, data=nearest, FUN = c))
#agg_nearest[agg_nearest$base != agg_nearest$nearest, ]
#agg_nearest[agg_nearest$base == 1593, ]
#dists_df[dists_df$base==1593, ]


# For each school in schools:
#   Find its 10(?) nearest neighbours
#   If these schools are less than 1(?) km from base school sum their flow numbers, and replace them with their centroid
