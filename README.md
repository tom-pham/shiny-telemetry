# Central Valley Acoustic Telemetry Project

[https://oceanview.pfeg.noaa.gov/shiny/FED/telemetry/](https://oceanview.pfeg.noaa.gov/shiny/FED/telemetry/)

## Background
This is a Shiny App developed for the Central Valley Acoustic Telemetry Project - an interagency collaborative effort between NOAA, University of California Santa Cruz, Bureau of Reclamation, USGS, USFWS, and CDFW.

The Juvenile Salmon Acoustic Telemetry System (JSATS) is used throughout the California Central Valley to track outmigrating juvenile salmon. Juvenile salmon are surgically implanted with acoustic tags that emit a unique signal which identifies an individual and are detected by underwater hydrophone receivers deployed throughout the Sacramento River all the way to the Golden Gate Bridge. Tagging efforts are conducted on both hatchery salmon as well as wild caught salmon. 

Management agencies can utilize detection and survival information to make decisions on water exports, while hatchery managers can utilize this information to make decisions on release strategies to maximize survival rates. 

## Application

Here we present retrospective data on tagging efforts by UCSC and NOAA, from the Southwest Fisheries Science Center since 2012, in an interactive format. 

### Receiver Deployments
Receivers are a fundamental part of JSATS. There are a number of different receiver types but in general we describe receivers in two types. Autonomous  receivers are those that are deployed in the river (or bay) and are either attached to a structure (such as a tree) or anchored by weights. They collect detections data on their own and must be physically retrieved in order to get the data from the device. The second type is real-time. Real-time receivers consists of a receiver board, modem, solar panel, battery, and a hydrophone that is enclosed in PVC conduit and fixed to a permanent structure such as a bridge. As their name implies - real-time receivers are capable of operating off of solar power and can upload data as it is received. 

The interactive map allows users to display receiver deployments by water year and receiver type. By clicking on a marker, receiver deployment data will populate into a table displaying information on the receiver name, serial number, and start/end times. 

### Hydrology
Hydrologic conditions is an important factor survival of outmigrating salmon. There is considerable variation year to year, within year, and geographically. Here we display river flow data (cubic feet / second) at 4 CDEC( California Data Exchange Center) stations: KES (Keswick Reservoir), BND (Bend Bridge), BTC ( Butte City), and WLK (Wilkins Slough). 

### Outmigration Animation
The outmigration animation visualizes fish outmigration using detection data over time. Unique fish detections at each receiver location are binned by day and the number of unique detections are overlaid in each point. The animation helps to demonstrate the movement and magnitude of fish across a broad landscape and differences between specific populations. 

### Data Explorer
Each study group consists of a cohort of fish. For each tagging effort, there is an attempt to select fish that are representative of the population. Weight and length measurements are collected. This section allows users to look at the weight and length distributions for each study group in a number of different ways: boxplots, histograms, density curves, and summary tables. They can be viewed individually or compared across groups. 

### Time of Day
Time of day allows users to visually explore behavioral differences in fish movement between night and day throughout the migratory corridor. Using detection times we can look at the distribution in movement times for an entire study group, or the movement times for a specific receiver location.

### Survival
Survival estimates are calculated by first collecting a detection history for a study group and then running it through a CJS survival model in RMark. Estimates are produced as reach specific per 10km or cumulative. We used a simple survival model where Phi = reach and p = reach. All available sites for a study group were utilized except for those in the Delta or in the Yolo Bypass. 

### Movement
Fish movement is summarized by study group in a summary table describing min/max and median travel time in days and km/day from release to each receiver location. In the future, we will update this page to display travel times as an interactive plot, and include a table/plot for per reach movement. 

#### Questions or Comments
If you have any questions or comments regarding this application please feel free to contact me at [tom.pham@noaa.gov](tom.pham@noaa.gov) and I will get back to you. 
