## Part 1

## Web scraping the data from the net

# Loading the required packages
library(tabulizer)
library(data.table)
library(httr)
library(rvest)
library(xml2)
library(leafletR)
library(RecordLinkage) # Package to match strings
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)
library(sf)
library(dplyr)

wd <- getwd()
## Extrating the table out of html page
link <- "https://www.firstpost.com/politics/lok-sabha-election-results-2019-state-constituency-party-wise-winning-candidates-full-winners-list-6697371.html"
html <- read_html(link)
table_nodes <- html_nodes(html,"table")
dt_results <- html_table(table_nodes)
dt <- as.data.table(dt_results)

## Naming the columns
for(i in c(1:7)){
  names(dt)[i] <- dt[[i]][1]
}

dt <- dt[-1]
rm(html,table_nodes,dt_results)

## Part2
## Cleaning the data

## Reading the shape files to match PC_NAME ans State names
shape <- readOGR("F:/Data_Analytics/Elections/maps-master/parliamentary-constituencies","india_pc_2019")
names(shape)[1] <- "STATE"
## Standardising the shapefiles data
shape$PC_NAME <- gsub("(.*)(\\(ST))","\\1",shape$PC_NAME)
shape$PC_NAME <- gsub("(.*)(\\(SC))","\\1",shape$PC_NAME)
shape$PC_NAME <- trimws(shape$PC_NAME)
shape$PC_CODE <- paste0(shape$PC_NAME,"_",shape$STATE)

dt[,PC_NAME:=toupper(Constituency)]
dt <- setorderv(dt,"PC_NAME")

pc_names_shape_files <- (sort(shape$PC_NAME))
pc_names_from_data <- sort(dt$PC_NAME)

check_names <- cbind(sort(pc_names_from_data[!(pc_names_from_data %in% pc_names_shape_files)])
                     , sort(pc_names_shape_files[!(pc_names_shape_files %in% pc_names_from_data)]))

check_names <- as.data.table(check_names)
names(check_names) <- c("data_name","shape_name")
fwrite(check_names,"F:/Data_Analytics/Elections/Parliament_2019/old_names.csv")

for(i in c(1:61)){
  l <- levenshteinSim(check_names[i,data_name],check_names$shape_name)
  l2 <- max(l)
  check_names[i,PC_new_name:= check_names$shape_name[l2==l]]
}

check_names[53,PC_new_name:=NA]
check_names[54,PC_new_name:="SARAN"]
setorderv(check_names,"PC_new_name")
check_names <- check_names[-1]

setorderv(dt,"PC_NAME")
for(i in c(1:60)){
  dt[PC_NAME==check_names$data_name[i],PC_NAME:=check_names$PC_new_name[i]]
}
# 
# dt$PC_NAME <- ifelse(pc_names_from_data %in% pc_names_shape_files,dt$PC_NAME,pc_names_shape_files) 
# dt[PC_NAME %in% check_names$data_name,PC_NAME:=check_names$PC_new_name]

## Verifying if the PC_NAMES are matching
pc_names_shape_files <- (sort(shape$PC_NAME))
pc_names_from_data <- sort(dt$PC_NAME)
pc_names_from_data[!(pc_names_from_data %in% pc_names_shape_files)]



p <- as.vector(shape$STATE)
q <- as.vector(shape$PC_NAME)
shape_dt <- as.data.table(cbind(p,q))
names(shape_dt) <- c("STATE","PC_NAME")

dt1 <- dt[,c(2,3,4,8)]
final_dt <- merge(shape_dt,dt1,by=c("PC_NAME"))
final_dt[,":="(count =.N),PC_NAME]
final_dt <- final_dt[count==1]


## Part 3 Plotting the data

dt <- final_dt
data <-as.data.frame(data)
missing_pc <- c("CHITTORGARH","BADAUN","MAHARAJGANJ","BHADOHI","PASCHIM CHAMPARAN",
"MAHARAJGANJ","AURANGABAD","NOWGONG","AURANGABAD","RAIGAD","VELLORE")

missing_state <- c("RAJASTHAN","UTTAR PRADESH","UTTAR PRADESH","UTTAR PRADESH","BIHAR",
                   "BIHAR","BIHAR","MADHYA PRADESH","MAHARASHTRA",
                   "MAHARASHTRA","TAMIL NADU")

                   
missing_candidates <- c("Shri C. P. Joshi","Dharmendra Yadav",
                        "Pankaj Choudhary","Ramesh Chand ",
                        "Sanjay Jaiswal","Janardan Singh Sigriwal",
                        "Susheel Kumar Singh","Pradyut Bordoloi",
                        "Imtiaz Jaleel Syed","Tatkare Sunil Dattatray","NA")

missing_party <- c("BJP","Samajwadi Party","BJP","BJP","BJP","BJP", "BJP","CONGRESS","AIMIM","NCP","NA")


dt_missing_data <- as.data.table(cbind(missing_pc,missing_state,missing_candidates,missing_party))
names(dt_missing_data) <- c("PC_NAME","STATE","Party","Winning Candidate")
dt_missing_data <- dt_missing_data[,.(PC_NAME,STATE,Party,`Winning Candidate`)]
data <- unique(dt[,.(PC_NAME,Party,STATE,`Winning Candidate`)])
data <- rbind(dt_missing_data,data)
unique(data[,Party])
data[,PC_CODE:=paste0(PC_NAME,"_",STATE)]
data[Party=="YSRC",Party:="YSR Congress"]
data[Party=="YSRCP",Party:="YSR Congress"]
data[Party=="CONGRESS",Party:="Congress"]
data <- unique(data[,.(PC_CODE,Party,`Winning Candidate`)])
data[,Party_popup:=Party]
major_parties <- c("YSR Congress","TMC","AAP","AIADMK","JDU",
                   "DMK","BJD","BJP","TRS","Congress","CPI",
                   "CPM","Shiv Sena","BSP","TDP","Samajwadi Party")

data[!(Party %in% major_parties),Party:="others"]
dt1 <- shape
names(dt1)
name <- paste0("2019_India_Parliament_election_Results By SAMINENI")

dt1 <- spTransform(dt1, CRS("+init=epsg:4326"))

dt1 <- merge(dt1,data,by=c("PC_CODE"),all.x = T)

dt1 <- spTransform(dt1, CRS("+init=epsg:4326"))

dt_data <- dt1@data[,c("PC_CODE","Party","Party_popup","STATE","Winning Candidate")] 
popup <- c("PC_CODE","Winning Candidate","Party_popup")

dt1 <- gSimplify(dt1,tol=0.1,topologyPreserve = TRUE)
dt1 <- SpatialPolygonsDataFrame(dt1,data=dt_data,match.ID = FALSE)

party_name <- c("YSR Congress","TMC","AAP","AIADMK","JDU","DMK","BJD","BJP","TRS",
                "Congress","CPI","CPM","Shiv Sena","BSP","TDP","Samajwadi Party","others")

party_color <- c("cadetblue1","darkblue","darkblue","gold","green","red",
                 "snow","orange","pink","purple","red","red","red","blue", "yellow",
                 "yellowgreen","gray1")

cuts <- party_name
writeOGR(dt1,paste0(wd,".geojson"),layer="",driver="GeoJSON")

sty <- styleCat(prop="Party", val=cuts,
                style.val= party_color, leg="Winner_party_colour (By SCV)")
map <- leaflet(data=paste0("F:/Data_Analytics",name,".geojson"),
               title=name, base.map="positron",style = sty,popup=popup,incl.data=TRUE) 



