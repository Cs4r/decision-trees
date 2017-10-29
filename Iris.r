library(readr)
iris <- read_csv("./Iris.csv")
View(iris)

# Remove column "id"
iris <- iris[2:6]

# Dimesions
dim(iris)

# Column datatypes
sapply(iris, class

# Summary
summary(iris)

# Scatter plots
library(Rcmdr)

# PetalLength vs PetalWidth

scatterplot(PetalWidthCm~PetalLengthCm | Species, reg.line=FALSE,
smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), by.groups=TRUE, data=iris)

# PetalLength vs SepalLength

scatterplot(SepalLengthCm~PetalLengthCm | Species, reg.line=FALSE,
smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), by.groups=TRUE, data=iris)

# PetalLength vs SepalWidth

scatterplot(SepalWidthCm~PetalLengthCm | Species, reg.line=FALSE,
smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), by.groups=TRUE, data=iris)

# PetalWidth vs SepalLength

scatterplot(SepalLengthCm~PetalWidthCm | Species, reg.line=FALSE,
smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), by.groups=TRUE, data=iris)

# PetalWidth vs SepalWidth

scatterplot(SepalWidthCm~PetalWidthCm | Species, reg.line=FALSE,
smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), by.groups=TRUE, data=iris)

# SepalLength vs SepalWidth
scatterplot(SepalLengthCm~SepalWidthCm | Species, reg.line=FALSE,
smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), by.groups=TRUE, data=iris


# Build the decision tree
library(C50)

# Source: https://stackoverflow.com/questions/21273492/how-to-plot-visualize-a-c50-decision-tree-in-r
C5.0.graphviz <- function( C5.0.model,   filename, fontname ='Arial',
                           col.draw ='black',col.font ='blue',col.conclusion ='lightpink',
                           col.question = 'grey78', shape.conclusion ='box3d',shape.question ='diamond',
                           bool.substitute = 'None', prefix=FALSE, vertical=TRUE ) {

    library(cwhmisc)
    library(stringr)
    treeout <- C5.0.model$output
    treeout<- substr(treeout,   cpos(treeout, 'Decision tree:', start=1)+14,nchar(treeout))
    treeout<- substr(treeout,   1,cpos(treeout, 'Evaluation on training data', start=1)-2)
    variables <- data.frame(matrix(nrow=500, ncol=4))
    names(variables) <- c('SYMBOL','TOKEN', 'TYPE' , 'QUERY')
    connectors <- data.frame(matrix(nrow=500, ncol=3))
    names(connectors) <- c('TOKEN', 'START','END')
    theStack <- data.frame(matrix(nrow=500, ncol=1))
    names(theStack) <- c('ITEM')
    theStackIndex <- 1
    currentvar <- 1
    currentcon <- 1
    open_connection <- TRUE
    previousindent <- -1
    firstindent <- 4
    substitutes <- data.frame(None=c('= 0','= 1'), yesno=c('no','yes'),
                              truefalse=c('false', 'true'),TF=c('F','T'))
    dtreestring<-unlist( scan(text= treeout,   sep='\n', what =list('character')))

    for (linecount in c(1:length(dtreestring))) {
        lineindent<-0
        shortstring <- str_trim(dtreestring[linecount], side='left')
        leadingspaces <- nchar(dtreestring[linecount]) - nchar(shortstring)
        lineindent <- leadingspaces/4
        dtreestring[linecount]<-str_trim(dtreestring[linecount], side='left')
        while (!is.na(cpos(dtreestring[linecount], ':   ', start=1)) ) {
            lineindent<-lineindent + 1
            dtreestring[linecount]<-substr(dtreestring[linecount],
                                           ifelse(is.na(cpos(dtreestring[linecount], ':   ', start=1)), 1,
                                                  cpos(dtreestring[linecount], ':   ', start=1)+4),
                                           nchar(dtreestring[linecount]) )
            shortstring <- str_trim(dtreestring[linecount], side='left')
            leadingspaces <- nchar(dtreestring[linecount]) - nchar(shortstring)
            lineindent <- lineindent + leadingspaces/4
            dtreestring[linecount]<-str_trim(dtreestring[linecount], side='left')
        }
        if (!is.na(cpos(dtreestring[linecount], ':...', start=1)))
            lineindent<- lineindent +  1
        dtreestring[linecount]<-substr(dtreestring[linecount],
                                       ifelse(is.na(cpos(dtreestring[linecount], ':...', start=1)), 1,
                                              cpos(dtreestring[linecount], ':...', start=1)+4),
                                       nchar(dtreestring[linecount]) )
        dtreestring[linecount]<-str_trim(dtreestring[linecount])
        stringlist <- strsplit(dtreestring[linecount],'\\:')
        stringpart <- strsplit(unlist(stringlist)[1],'\\s')
        if (open_connection==TRUE) {
            variables[currentvar,'TOKEN'] <- unlist(stringpart)[1]
            variables[currentvar,'SYMBOL'] <- paste('node',as.character(currentvar), sep='')
            variables[currentvar,'TYPE'] <- shape.question
            variables[currentvar,'QUERY'] <- 1
            theStack[theStackIndex,'ITEM']<-variables[currentvar,'SYMBOL']
            theStack[theStackIndex,'INDENT'] <-firstindent
            theStackIndex<-theStackIndex+1
            currentvar <- currentvar + 1
            if(currentvar>2) {
                connectors[currentcon - 1,'END'] <- variables[currentvar - 1, 'SYMBOL']
            }
        }
        connectors[currentcon,'TOKEN'] <- paste(unlist(stringpart)[2],unlist(stringpart)[3])
        if (connectors[currentcon,'TOKEN']=='= 0')
            connectors[currentcon,'TOKEN'] <- as.character(substitutes[1,bool.substitute])
        if (connectors[currentcon,'TOKEN']=='= 1')
            connectors[currentcon,'TOKEN'] <- as.character(substitutes[2,bool.substitute])
        if (open_connection==TRUE) {
            if (lineindent<previousindent) {
                theStackIndex <- theStackIndex-(( previousindent- lineindent)  +1 )
                currentsymbol <-theStack[theStackIndex,'ITEM']
            } else
                currentsymbol <-variables[currentvar - 1,'SYMBOL']
        } else {
            currentsymbol <-theStack[theStackIndex-((previousindent -lineindent ) +1    ),'ITEM']
            theStackIndex <- theStackIndex-(( previousindent- lineindent)    )
        }
        connectors[currentcon, 'START'] <- currentsymbol
        currentcon <- currentcon + 1
        open_connection <- TRUE
        if (length(unlist(stringlist))==2) {
            stringpart2 <- strsplit(unlist(stringlist)[2],'\\s')
            variables[currentvar,'TOKEN']   <- paste(ifelse((prefix==FALSE),'','Class'), unlist(stringpart2)[2])
            variables[currentvar,'SYMBOL']  <- paste('node',as.character(currentvar), sep='')
            variables[currentvar,'TYPE']        <- shape.conclusion
            variables[currentvar,'QUERY']   <- 0
            currentvar <- currentvar + 1
            connectors[currentcon - 1,'END'] <- variables[currentvar - 1,'SYMBOL']
            open_connection <- FALSE
        }
        previousindent<-lineindent
    }
    runningstring <- paste('digraph g {', 'graph ', sep='\n')
    runningstring <- paste(runningstring, ' [rankdir="', sep='')
    runningstring <- paste(runningstring, ifelse(vertical==TRUE,'TB','LR'), sep='' )
    runningstring <- paste(runningstring, '"]', sep='')
    for (lines in c(1:(currentvar-1))) {
        runningline <- paste(variables[lines,'SYMBOL'], '[shape="')
        runningline <- paste(runningline,variables[lines,'TYPE'], sep='' )
        runningline <- paste(runningline,'" label ="', sep='' )
        runningline <- paste(runningline,variables[lines,'TOKEN'], sep='' )
        runningline <- paste(runningline,
                             '" style=filled fontcolor=', sep='')
        runningline <- paste(runningline, col.font)
        runningline <- paste(runningline,' color=' )
        runningline <- paste(runningline, col.draw)
        runningline <- paste(runningline,' fontname=')
        runningline <- paste(runningline, fontname)
        runningline <- paste(runningline,' fillcolor=')
        runningline <- paste(runningline,
                             ifelse(variables[lines,'QUERY']== 0 ,col.conclusion,col.question))
        runningline <- paste(runningline,'];')
        runningstring <- paste(runningstring,   runningline , sep='\n')
    }
    for (lines in c(1:(currentcon-1)))        {
        runningline <- paste (connectors[lines,'START'], '->')
        runningline <- paste (runningline, connectors[lines,'END'])
        runningline <- paste (runningline,'[label="')
        runningline <- paste (runningline,connectors[lines,'TOKEN'], sep='')
        runningline <- paste (runningline,'" fontname=', sep='')
        runningline <- paste (runningline, fontname)
        runningline <- paste (runningline,'];')
        runningstring <- paste(runningstring,   runningline , sep='\n')
    }
    runningstring <- paste(runningstring,'}')
    sink(filename, split=TRUE)
    cat(runningstring)
    sink()
}

setosa = as.data.frame(subset(iris, iris$Species == "Iris-setosa"))
versicolor = as.data.frame(subset(iris, iris$Species == "Iris-versicolor"))
virginica = as.data.frame(subset(iris, iris$Species == "Iris-virginica"))

decisionTree <- function(setosa, versicolor, virginica, modelNumber) {
  trainData <- rbind(setosa[1:33, ], versicolor[1:33, ], virginica[1:33, ])
  testData <- rbind(setosa[34:50, ], versicolor[34:50, ], virginica[34:50, ])

  # Create a training set and a test set for X and y.
  trainX <- trainData[,1:4]
  trainy <-  as.factor(trainData[,5])

  testX <- testData[,1:4]
  testy <- as.factor(testData[,5])

  # Build the model
  model <- C50::C5.0( trainX, trainy, rules=TRUE ) # Firstly with rules
  print(summary)
  model <- C50::C5.0( trainX, trainy)
  print(summary(model))

  C5.0.graphviz(model, paste("./iris-model-", modelNumber, ".txt", sep =""), col.question ='cyan')

  # Error rate
  p <- predict( model, testX, type="class" )

  return (100*(1 - sum( p == testy ) / length( p )))
}

# Fix the seed so that the experiment is reproducible
set.seed(42);

experiment <- function(number, randomize = TRUE) {

  if(randomize){
    setosa <- setosa[ sample( nrow( setosa ) ), ]
    versicolor <- versicolor[ sample( nrow( versicolor ) ), ]
    virginica <- virginica[ sample( nrow( virginica ) ), ]
  }

  errorN = decisionTree(setosa, versicolor, virginica, number)

  cat(paste("\nClassification error: ", errorN))
}

# Experiments
# Training data has 2/3 of cases, test data has 1/3

# First try: Original data order
experiment(1, FALSE)

# Second try: Shuffle original data
experiment(2)

# Third try: Shuffle original data
experiment(3)

# Fourth try: Shuffle original data
experiment(4)

# Fifth try: Shuffle original data
experiment(5)

# Sixth try: Shuffle original data
experiment(6)

# Seventh try: Shuffle original data
experiment(7) # classification error in training data: 3.0, classification error in test data: 1.96
