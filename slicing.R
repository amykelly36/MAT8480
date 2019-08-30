# import data
data<-read.csv("pva97nko.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

# select a value
data[1,"TargetB"]

# select multiple rows
demo<-data[1:20,]

# select multiple columns by name using c()
demo[,c("TargetB", "GiftCnt36", "GiftCntAll")]
demo[c("TargetB", "GiftCnt36", "GiftCntAll")]

# select multiple columns by numeric indices
demo[1:3]
demo[c(1,2,3)]
demo[-c(1,3)]

# subset data with row and column indices
demo[1:10, c("TargetB", "GiftCnt36")]

# set conditions to filter data
demo[demo$DemGender=="F",1:5]
demo[demo$GiftCnt36<2, 1:5]

# which function returns the indexes of satisfied data
which(demo$DemGender=="F")


# logical operators
# == means ‘is equal to’
# != means ‘is not equal to’
# < means ` is less than’
# > means ` is greater than’
# <= means ` is less than or equal to’
# >= means ` is greater than or equal to’

