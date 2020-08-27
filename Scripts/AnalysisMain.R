library(BehaviouR)
library(ggplot2)
library(patchwork)
FrameRate <- 25
MouseBodyList <- list()
MouseBodyList$head <- c("ear", "nose")
MouseBodyList$body <- c("ear", "tail")
ObjectList <- list("objectA", "objectB")
DataSet <- DeepLabCutLoad(FileName = "output_files/Odortest_mp4_convertedDLC_resnet50_Odor test WinterAug22shuffle1_350000.csv",
                          FrameRate = FrameRate,
                          MouseLabels = MouseBodyList,
                          ObjectLabels = ObjectList,
                          ObjectNumber = 2,
                          xScale = 50/500,
                          yScale = 50/530, 
                          JumpCorrections = T,
                          includeAll = F)


# compute body motion properties
LocationDensity <- LocationPlot(DataTable = DataSet$DataTable,
             ObjectTable = DataSet$ObjectTable,
             Density = T,
             CoordRef = "bodyCentroid")

LocationTrace<- LocationPlot(DataTable = DataSet$DataTable,
                                ObjectTable = DataSet$ObjectTable,
                                Density = F,
                                CoordRef = "bodyCentroid")

LocationArrange <- LocationTrace + LocationDensity & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/LocationArrange.png", plot = LocationArrange, device = "png", dpi = 400, width = 10, height = 5)


DistSpeedCalc(CoordTable = DataSet$DataTable,
              SpeedRef = "bodyCentroid",
              Interval = 1/FrameRate)

CoordCumDistance <- DistancePlot(DataTable = DataSet$DataTable,
                                 Distance = "CumDistbodyCentroid",
                                 CoordRef = "bodyCentroid",
                                 ObjectTable = DataSet$ObjectTable,
                                 Unit = "cm")

CumulativeDistance <- DistancePlot(DataTable = DataSet$DataTable,
                                   Distance = "CumDistbodyCentroid",
                                   ObjectTable = DataSet$ObjectTable,
                                   Unit = "cm")

CumulativeDistanceArrange <- CoordCumDistance + CumulativeDistance & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/CumulativeDistanceArrange.png", plot = CumulativeDistanceArrange, device = "png", dpi = 400, width = 10, height = 5)

# add centroid between the ears
AddCentroid(CoordTable = DataSet$DataTable,
            CornerNames = "Ear",
            OutputName = "BetweenEars")

# calculate the length of the body to detect rearing
VectorLength(CoordTable = DataSet$DataTable,
             VectorStart = "TailBase",
             VectorEnd = "BetweenEars",
             OutputName = "BodyLength")

RearingPlotCoord <- LengthPlot(DataTable = DataSet$DataTable,
                          Length = "BodyLength",
                          CoordRef = "bodyCentroid",
                          ObjectTable = DataSet$ObjectTable,
                          Unit = "cm") +
  scale_color_viridis_c(option = "magma",guide = guide_colourbar(title = "Normalised\nRearing", label = FALSE, ticks = F), direction = -1)

RearingPlot <- LengthPlot(DataTable = DataSet$DataTable,
                               Length = "BodyLength",
                               ObjectTable = DataSet$ObjectTable,
                               Unit = "cm")

RearingArrange <- RearingPlotCoord + RearingPlot & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/RearingArrange.png", plot = RearingArrange, device = "png", dpi = 400,  width = 10, height = 5)
LengthPlot(DataTable = DataSet$DataTable[BodyLength!=max(BodyLength),,],
           Length = "BodyLength",
           ObjectTable = DataSet$ObjectTable,
           Unit = "cm")

# reset outliers
DataSet$DataTable[BodyLength>30,BodyLength:=mean(DataSet$DataTable$BodyLength),]

RearingPlotCoord <- LengthPlot(DataTable = DataSet$DataTable,
                               Length = "BodyLength",
                               CoordRef = "bodyCentroid",
                               ObjectTable = DataSet$ObjectTable,
                               Unit = "cm") +
  scale_color_viridis_c(option = "magma",guide = guide_colourbar(title = "Normalised\nRearing", label = FALSE, ticks = F), direction = -1)

RearingPlot <- LengthPlot(DataTable = DataSet$DataTable,
                          Length = "BodyLength",
                          ObjectTable = DataSet$ObjectTable,
                          Unit = "cm")

RearingArrangeOutlierOut <- RearingPlotCoord + RearingPlot & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))
ggsave(filename = "PresentationFile-figure/RearingArrangeOutlierOut.png", plot = RearingArrangeOutlierOut, device = "png", dpi = 400,  width = 10, height = 5)



AngleCalc(CoordTable = DataSet$DataTable,
          VectorStart1 = "BetweenEars",
          VectorEnd1 = "Nose",
          VectorStart2 = "TailBase",
          VectorEnd2 = "BetweenEars",
          OutputName = "HeadAngle")

AnglePlotLight <- AnglePlot(DataTable = DataSet$DataTable,
          Angle = "HeadAngle",
          CoordRef = "bodyCentroid",
          ObjectTable = DataSet$ObjectTable,
          colourScheme = "light")
AnglePlotDark <- AnglePlot(DataTable = DataSet$DataTable,
                            Angle = "HeadAngle",
                            CoordRef = "bodyCentroid",
                            ObjectTable = DataSet$ObjectTable,
                            colourScheme = "dark")
AngleArrange <- AnglePlotLight + AnglePlotDark & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/AngleArrange.png", plot = AngleArrange, device = "png", dpi = 400,  width = 10, height = 5)

SpeedCoord <- SpeedPlot(DataTable = DataSet$DataTable,
          Speed = "SpeedbodyCentroid",
          CoordRef = "bodyCentroid",
          ObjectTable = DataSet$ObjectTable,
          Unit = "cm/s")

Speed <- SpeedPlot(DataTable = DataSet$DataTable,
                        Speed = "SpeedbodyCentroid",
                        ObjectTable = DataSet$ObjectTable,
                        Unit = "cm/s")

SpeedArrange <- SpeedCoord + Speed & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/SpeedArrange.png", plot = SpeedArrange, device = "png", dpi = 400,  width = 10, height = 5)


ObjectDistance(CoordTable = DataSet$DataTable,
               ObjectTable = DataSet$ObjectTable,
               Ref = "headCentroid")

DistanceObjectA <- DistancePlot(DataTable = DataSet$DataTable, Distance = "objectA_headCentroid_Distance", ObjectTable = DataSet$ObjectTable, Unit = "cm")
DistanceObjectB <- DistancePlot(DataTable = DataSet$DataTable, Distance = "objectB_headCentroid_Distance", ObjectTable = DataSet$ObjectTable, Unit = "cm")

DistanceObjectArrange <- DistanceObjectA / DistanceObjectB & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/DistanceObjectArrange.png", plot = DistanceObjectArrange, device = "png", dpi = 400,  width = 10, height = 5)


ggplot(data = DataSet$DataTable, aes(x=BodyLength))+ 
  geom_density()+
  labs(title = "Animal Length", subtitle = "Length from head to tailbase, excluding the tail.", x = "Length (cm)", y = "Density")+
  theme_minimal()


ggplot(data = DataSet$DataTable, aes(x=SpeedbodyCentroid))+ 
  geom_density()+
  labs(title = "Animal Speed", subtitle = "Speed calculation based on the body centroid.", x = "Speed (cm/s)", y = "Density")+
  theme_minimal()


DistancePlot(DataTable = DataSet$DataTable, Distance = "objectA_headCentroid_Distance", Unit = "cm")

ObjectAngle(CoordTable = DataSet$DataTable, ObjectTable = DataSet$ObjectTable, RefStart = "BetweenEars", Ref = "Nose")

# show highlighting function
AnglePlotAlpha <- AnglePlot(DataTable = DataSet$DataTable,
          Angle = "objectB_Nose_Angle",
          CoordRef = "headCentroid",
          ObjectTable = DataSet$ObjectTable,
          ObjectHighlight = "alpha",
          colourScheme = "light")

AnglePlotStroke <- AnglePlot(DataTable = DataSet$DataTable,
          Angle = "objectB_Nose_Angle",
          CoordRef = "headCentroid",
          ObjectTable = DataSet$ObjectTable,
          ObjectHighlight = "stroke",
          colourScheme = "light")

AnglePlotColour <- AnglePlot(DataTable = DataSet$DataTable,
          Angle = "objectB_Nose_Angle",
          CoordRef = "headCentroid",
          ObjectTable = DataSet$ObjectTable,
          ObjectHighlight = "colour",
          colourScheme = "light")


AnglePlotGreen <- AnglePlot(DataTable = DataSet$DataTable,
          Angle = "objectB_Nose_Angle",
          CoordRef = "headCentroid",
          ObjectTable = DataSet$ObjectTable,
          ObjectHighlight = "green",
          colourScheme = "light")

AngleObjectArrange <- (AnglePlotAlpha + AnglePlotStroke) / (AnglePlotColour + AnglePlotGreen) & plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))

ggsave(filename = "PresentationFile-figure/AngleObjectArrange.png", plot = AngleObjectArrange, device = "png", dpi = 400,  width = 10, height = 7)


ZoneEntry(CoordTable = DataSet$DataTable,
          DistanceRef = "objectA_headCentroid_Distance",
          Length = 5,
          AngleInclusion = F,
          AngleRef = "objectA_Nose_Angle",
          AngleRange = pi*(30/180))

ZoneEntry(CoordTable = DataSet$DataTable,
          DistanceRef = "objectB_headCentroid_Distance",
          Length = 5,
          AngleInclusion = F,
          AngleRef = "objectB_Nose_Angle",
          AngleRange = pi*(30/180))

max(cumsum(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry))
max(cumsum(DataSet$DataTable$objectB_headCentroid_Distance_inArea_entry))

EntryData <- data.table::data.table(EntryCount = c(cumsum(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry),
                                      cumsum(DataSet$DataTable$objectB_headCentroid_Distance_inArea_entry)),
                       ObjectName = c(rep_len("A", length(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry)),
                                      rep_len("B", length(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry))),
                       Time = rep(x = DataSet$DataTable$Time, time = 2))

ZoneEntries <- ggplot(data = EntryData, aes(x=Time, y = EntryCount, Group = ObjectName, colour = ObjectName))+ 
  geom_line()+
  scale_colour_viridis_d()+
  labs(title = "Zone-Entries", x = "Time (s)", y = "Entries", colour = "Object")+
  theme_minimal()

ggsave(filename = "PresentationFile-figure/ZoneEntries.png", plot = ZoneEntries, device = "png", dpi = 400,  width = 10, height = 7)

DataSet$DataTable[objectB_headCentroid_Distance_inArea_entry==1,Time]
testData <- data.table::data.table(ObjectName = c(rep_len("A", length(DataSet$DataTable[objectA_headCentroid_Distance_inArea_entry==1,diff(Time)])),
                                                   rep_len("B", length(DataSet$DataTable[objectB_headCentroid_Distance_inArea_entry==1,diff(Time)]))),
                       TimeDiff = c(DataSet$DataTable[objectA_headCentroid_Distance_inArea_entry==1,diff(Time)], DataSet$DataTable[objectB_headCentroid_Distance_inArea_entry==1,diff(Time)]))
testData[,Time:=cumsum(TimeDiff),by=ObjectName]

IntervalEst <- brms::brm(formula = TimeDiff ~ ObjectName+ObjectName:Time, data = testData, family = Gamma(link = "log"))



EntryDataBin <- data.table::data.table(Entry = c(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry,
                                                   DataSet$DataTable$objectB_headCentroid_Distance_inArea_entry),
                                    ObjectName = c(rep_len("A", length(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry)),
                                                   rep_len("B", length(DataSet$DataTable$objectA_headCentroid_Distance_inArea_entry))),
                                    Time = rep(x = DataSet$DataTable$Time, time = 2))

library(brms)
testT <- brms::brm(formula = Entry ~ ObjectName+ObjectName:Time, data = EntryDataBin, family = brms::bernoulli(link = "logit"))
plot(testT)
