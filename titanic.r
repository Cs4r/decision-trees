library(readr)
titanic <- read_csv("./titanic.csv")
View(titanic)

# Attributes / Column names
names(titanic)

# Dimensions
dim(titanic)

# Check missing values
sum((is.na(titanic["CLASS"])))
sum((is.na(titanic["AGE"])))
sum((is.na(titanic["SEX"])))
sum((is.na(titanic["SURVIVED"])))

# Replace non-ASCII characters to ASCII characters
unique(titanic$AGE)
titanic$AGE[titanic$AGE != "Adulto" ] <- "Ninio"

# Distribution of attributes

# via bar charts
par(mfrow =c(2,2))
barplot(table(titanic$CLASS), main = "CLASS Distribution")
barplot(table(titanic$AGE), main = "AGE Distribution")
barplot(table(titanic$SEX), main = "SEX Distribution")
barplot(table(titanic$SURVIVED), main = "SURVIVED Distribution")

# via pie charts
par(mfrow =c(2,2))
pie(table(titanic$CLASS), main = "CLASS Distribution")
pie(table(titanic$AGE), main = "AGE Distribution")
pie(table(titanic$SEX), main = "SEX Distribution")
pie(table(titanic$SURVIVED), main = "SURVIVED Distribution")

# frequency tables
library(plyr)
count(titanic$CLASS)
count(titanic$AGE)
count(titanic$SEX)
count(titanic$SURVIVED)

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
    cat(runningstring)
    sink(filename, split=TRUE)
    cat(runningstring)
    sink()
}


# First try: Training data has 2/3 of cases, test data has 1/3

positive = as.data.frame(subset(titanic, titanic$SURVIVED == "Sobrevive"))
negative = as.data.frame(subset(titanic, titanic$SURVIVED == "Muere"))

trainData <- rbind(positive[1:474, ], negative[1:993, ])
testData <- rbind(positive[475:711, ], negative[994:1490, ])

# Both (training and test data) have same proportion of positive/negative cases
# as the original input
nrow(subset(titanic, titanic$SURVIVED == "Sobrevive"))/nrow(titanic)
nrow(subset(trainData, trainData$SURVIVED == "Sobrevive"))/nrow(trainData)
nrow(subset(testData, testData$SURVIVED == "Sobrevive"))/nrow(testData)

# Create a training set and a test set for X and y.
trainX <- trainData[,1:3]
trainy <-  as.factor(trainData[,4])

testX <- testData[,1:3]
testy <- as.factor(testData[,4])

# Build the model
model <- C50::C5.0( trainX, trainy )
summary(model)
C5.0.graphviz(model, './model-1.txt', col.question ='cyan')

# Error rate
p <- predict( model, testX, type="class" )
100*(1 - sum( p == testy ) / length( p ))



# Fix the seed so that the experiment is reproducible
set.seed(42);

# Second try: Random sample

titanic1 <- titanic[ sample( nrow( titanic ) ), ]

X <- as.data.frame(titanic1[,1:3])
y <- as.data.frame(titanic1[,4])

trainX <- X[1:1467,]
trainy <- as.factor(y[1:1467,])

testX <- X[1468:2201,]
testy <- as.factor(y[1468:2201,])

# Build the model
model <- C50::C5.0( trainX, trainy )
summary(model)
C5.0.graphviz(model, './model-2.txt', col.question ='cyan')

# Error rate
p <- predict( model, testX, type="class" )
100*(1 - sum( p == testy ) / length( p ))

# Third try: Random sample

titanic2 <- titanic[ sample( nrow( titanic ) ), ]

X <- as.data.frame(titanic2[,1:3])
y <- as.data.frame(titanic2[,4])

trainX <- X[1:1467,]
trainy <- as.factor(y[1:1467,])

testX <- X[1468:2201,]
testy <- as.factor(y[1468:2201,])

# Build the model
model <- C50::C5.0( trainX, trainy )
summary(model)
C5.0.graphviz(model, './model-3.txt', col.question ='cyan')

# Error rate
p <- predict( model, testX, type="class" )
100*(1 - sum( p == testy ) / length( p ))
