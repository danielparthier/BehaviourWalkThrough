library(BehaviouR)
FrameRate <- 25/3 
MouseBodyList <- list()
MouseBodyList$head <- c("ear", "nose")
MouseBodyList$body <- c("ear", "tail")
ObjectList <- list("object")
DataSet <- DeepLabCutLoad(FileName = "output_files/Odortest_03022018_b13_r238DLC_resnet50_Odor test WinterAug22shuffle1_350000.csv",
                          FrameRate = 30,
                          MouseLabels = MouseBodyList,
                          ObjectLabels = ObjectList,
                          ObjectNumber = 2,
                          JumpCorrections = T,
                          includeAll = F)



# compute body motion properties
DistSpeedCalc(CoordTable = DataSet$DataTable,
              SpeedRef = "bodyCentroid",
              Interval = 1/FrameRate)

AddCentroid(CoordTable = DataSet$DataTable,
            CornerNames = "Ear",
            OutputName = "BetweenEars")

VectorLength(CoordTable = DataSet$DataTable,
             VectorStart = "TailBase",
             VectorEnd = "BetweenEars",
             OutputName = "BodyLength")

