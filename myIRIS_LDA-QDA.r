
print("hello Greg...welcome to R")
library('MASS')
#library('mda') # for mixed model discriminants - need R>v3.5
library('ggplot2')
library('dplyr') # for resampling
library('caret') # for confusion matrix
data = read.table('/home/microcarbon/Desktop/iris_tab.txt', header = TRUE)
Sp = data$species; # species
SL = data$sepal_length; # sepal length
SW = data$sepal_width; # sepal width
PL = data$petal_length; # petal length
PW = data$petal_width; # petal width
dataframe = data.frame(SL, SW, PL, PW, Sp)
subframe = data.frame(SL, SW, PL, PW)
print (subframe)
rnd <- sample(1:150, 1)
set.seed(rnd)
mysample = sample_n(dataframe, 50, replace=FALSE)
print(mysample)
#sample_subframe = mysample(SL, SW, PL, PW)
#print(sample_subframe)
#mytest <-  lda(Sp~SL+SW+PL+PW, mysample) # linear discriminant analysis
mytest <-  qda(Sp~SL+SW+PL+PW, mysample) # quadratic discriminant analysis
#mytest <-  mda(Sp~SL+SW+PL+PW, mysample) # mixed model discriminant analysis (using GMM)
mypred <- predict(mytest, subframe, prior = mytest$prior, method = c("plug-in", "predictive", "debiased"))
print(mytest)
print(mypred)
print(mypred$class)
mypred_class = mypred$class
myplot1 <-ggplot(mysample, aes(PL, PW, colour = Sp)) + geom_point()
myplot2 <-ggplot(mysample, aes(SL, SW, colour = Sp)) + geom_point()                                 
myplot3 <-ggplot(subframe, aes(PL, PW, colour = Sp)) + geom_point()
myplot4 <-ggplot(subframe, aes(SL, SW, colour = Sp)) + geom_point()                                 
myLDA <- summary(mytest)
print(myLDA)
myplot5 <-ggplot(subframe, aes(PL, PW, colour = mypred_class)) + geom_point()                                 
myplot6 <-ggplot(subframe, aes(SL, SW, colour = mypred_class)) + geom_point()                                 
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(myplot5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(myplot6, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# confusion matrix
print(as.factor(mypred_class))
print(as.factor(Sp))
mymatrix <- confusionMatrix(as.factor(mypred_class), as.factor(Sp))
print(mymatrix)
#LD plot
myLDplot <- plot(mytest)
print(myLDplot)

print("goodbye Greg...leaving R")
#q()