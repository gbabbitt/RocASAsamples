
print("hello Greg...welcome to R")

library('ggplot2')
library('tripack') # for voronoi graph
library('RColorBrewer') # for voronoi graph

data = read.table('/home/microcarbon/Desktop/iris_tab.txt', header = TRUE)
Sp = data$species; # species
SL = data$sepal_length; # sepal length
SW = data$sepal_width; # sepal width
PL = data$petal_length; # petal length
PW = data$petal_width; # petal width
dataframe = data.frame(SL, SW, PL, PW, Sp)
print (dataframe)
subframe = data.frame(SL, SW, PL, PW)
set.seed(2)
# NOTE: for iris data set centers = 3 species
# ...to see voronoi patterns set centers >> 3
mytest <-  kmeans(subframe, centers = 3, nstart = 1, algorithm = "Lloyd")
myplot1 <-ggplot(data, aes(PL, PW, color = Sp)) + geom_point()
myplot3 <-ggplot(data, aes(SL, SW, color = Sp)) + geom_point()                                 
myKMEANS <- summary(mytest)
print(myKMEANS)
mytest$cluster <- as.factor(mytest$cluster)
myplot2 <-ggplot(data, aes(PL, PW, color = mytest$cluster)) + geom_point()                                 
myplot4 <-ggplot(data, aes(SL, SW, color = mytest$cluster)) + geom_point()                                 
library('grid')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# voronoi plot for sepals NOTE: full plot needs 4 or more centers
CL5 <- brewer.pal(5, "Pastel1")
plot(SW~SL,pch=19,col=CL5[mytest$cluster])
print(mytest$centers)
V <- voronoi.mosaic(mytest$centers[,1],mytest$centers[,2])
P <- voronoi.polygons(V)
points(mytest$centers[,1],mytest$centers[,2],pch=3,cex=1.5,lwd=2)
plot(V, add=TRUE)

# voronoi plot for petals NOTE: full plot needs 4 or more centers
CL5 <- brewer.pal(5, "Pastel1")
plot(PW~PL,pch=19,col=CL5[mytest$cluster])
print(mytest$centers)
V <- voronoi.mosaic(mytest$centers[,3],mytest$centers[,4])
P <- voronoi.polygons(V)
points(mytest$centers[,3],mytest$centers[,4],pch=3,cex=1.5,lwd=2)
plot(V,add=TRUE)

print("goodbye Greg...leaving R")
#q()