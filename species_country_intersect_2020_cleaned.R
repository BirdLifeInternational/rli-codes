# pp species per country calculator v1.  26/06/2016
# Maria Dias(BirdLife International), updated by Ashley Simkins (BirdLife International) 16/03/2020
# Script to estimate the percentage of each species' distribution range overlapping with each country and EEZ

### IMPORTANT NOTES
# Given the complexity of the code, it is strongly recommended to copy-paste the code into an R editor (e.g. R-studio)
# The script was written assuming that the analyses will run in a conventional PC (e.g 16 GB RAM); analyses should be SPLIT PER BATCHES
# Some shapefiles (e.g. MAMMALS) can take several minutes to upload

### TODO Before running this code, you need to read the instructions in Appendix 1 of the RLI Report. This will explain what files you need to run the code, what format the files should be in, any steps you need to take prior to running it, and finally describing how to run the code and what to change in the below.

## Part 1 - load in packages, input files and set folder and working directories

### packages to install
kpacks <- c('sp','maptools', 'rgdal', 'geosphere','fields','spatstat','maps','rgeos','data.table', 'raster', 'tidyverse', 'sf', 'lwgeom')  ### install automatically the relevant packages
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)
####################################################

year_run <- format(Sys.Date(), "%Y") #get the current year

### TODO set directories
folder <- "C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/input files country intersection" ## copy paste here the working folder - where the csv input files are stored
setwd(folder)

shapesc <- 'C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/shapefiles_RLI'   # folder with the shapefiles with species countries and EEZs  (copy-paste the path if different from the one defined in "folder")

shapesSp <- 'C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/shapefiles_RLI/species_to_run_RLI_2019_noPOS'

if (file.exists(paste('../files batches', year_run))){ #create folder in which to store the output files
  finfolder <- paste('../files batches', year_run)
} else {
  dir.create(file.path(paste('../files batches', year_run)))
  finfolder <- paste('../files batches', year_run)
}

linkiso <- read_csv("Iso_countries_2019.csv") #list of countries and iso codes
marinesp <- read_csv("marine.csv") ## list of species that use marine ecosystems

## Optional depending on what you are running
# Use (uncomment - remove the #) if you have a list of endemic species
#endemics <- read_csv("endemic.csv") ## list of endemic species

#load in country and eez layers
cntsw <- st_read(dsn = paste(shapesc, '/Gadm_28_adm0', sep = ''), layer="gadm28_adm0", stringsAsFactors = F, geometry_column = T) #reads the Countries polygon layer
eezw <- st_read(dsn = paste(shapesc, '/World_EEZ_v10_20180221', sep = ''), layer="eez_v10_wo_joint_diss") #reads the EEZ polygon layer

##############################

##  Part 2 - define functions needed for overlap analyses

## custom functions
getVertices <- function(x){
  stopifnot("sf" %in% class(x))
  n <- nrow(x[[1]])
  vts <- data.frame()
  for (z in 1:n){
    a <- lapply(x@polygons, function(y) {y@Polygons[[z]]@coords})
    vts <- rbind(vts,data.frame(x = a[[1]][,1], y = a[[1]][,2])) 
    }
  return(vts)
  }

shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
    proj.to = NA, map = TRUE) {
    require(raster, rgdal)
    # use transform==TRUE if the polygon is not in the same coordinate system as
    # the output raster, setting proj.from & proj.to to the appropriate
    # projections
    if (transform == TRUE) {
        proj4string(shp) <- proj.from
        shp <- spTransform(shp, proj.to)
    }
    # convert the shapefile to a raster based on a standardised background
    # raster
    r <- raster::rasterize(shp, mask.raster)
    # set the cells associated with the shapfile to the specified value
    r[!is.na(r)] <- value
    # merge the new raster with the mask raster and export to the working
    # directory as a tif file
    #r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
    #    overwrite = T)
    # plot map of new raster
    if (map == TRUE) {
        plot(r, main = label, axes = F, box = F)
    }
    names(r) <- label
    return(r)
}

lu <- function (x=x) length(unique(x))
first <- function (x=x) x[1]

## to fix country/eez polygons without ISO - see first in a GIS software if input shape is different
cntsw[is.na(cntsw$ISO),] #lists all countries without isos in the country layer

eeziso <- na.omit(eezw[,c("Sovereign1","ISO_Ter1")]) #only use first territory for disputed territories
eezw[is.na(eezw$ISO_Ter1),] #lists all countryies without isos in the EEZ layer

nnulls <- nrow(eezw[is.na(eezw$ISO_Ter1),]) #check if any countries missing ISO codes

eezw <- eezw[!is.na(eezw$ISO_Ter1),]   ### excludes EEZ polygons without ISO3 (disputed/jointed areas; 12 in 232 polygons)
#eezw <- eezw[eezw$Pol_type!= 'Joint regime',] #filter out sites with unclear ownership
length(unique(eezw$ISO_Ter1)) #check how many unique iso codes there are

cntab <- data.frame(ISO3 = cntsw$ISO, Areakm = as.numeric(st_area(cntsw)))
cntab$id <- 1:nrow(cntab)
str(cntab)
nrow(cntsw)
eeztab <- data.frame(ISO3 = eezw$ISO_Ter1, Areakm = as.numeric(st_area(eezw)))
eeztab$id <- 1:nrow(eeztab)
str(eeztab)
nrow(eezw)


# Part 3 - DECIDE HERE WHICH BATCH TO RUN by what shapefiles are in the filepath defined at the start as shapesSp
#load in species layer of species to run in batches (via a loop)

batch_test <- list.files(shapesSp, pattern = ".shp$")

for (r in 1:length(batch_test)){
  #r <- 4
  tabsp <- as.character(batch_test[r]) #call the file tabsp
  tabsp <- tools::file_path_sans_ext(tabsp) #drop the file extension (drop .shp)
  
  print(tabsp)
  
  spTr <- st_read(dsn = shapesSp, layer = tabsp, stringsAsFactors = F) #read in species shapefiles 
  str(spTr)
  nrow(spTr)
  
  #############################################################
  
  if (file.exists(paste(finfolder, tabsp, sep = "/"))){# set folder where the files of this batch will be saved - the folder should exist and be within the working folder, if it doesn't then this is created here
    finfolder2 <- paste(finfolder, tabsp, sep="/")
  } else {
    dir.create(file.path(paste(finfolder, tabsp, sep = "/")))
    finfolder2 <- paste(finfolder, tabsp, sep="/")
  }
  
  
  tab2use <- spTr[!duplicated('binomial', fromLast = T),] #if more than one year for a species, take the most recent year of assessment
  
  nrow(spTr) - nrow(tab2use) #should be 0 if species only have one map in the spatial data
  
  #spTr$area <- st_area(spTr$geometry) #get the area of each species' range
  
  tab2use$marine <- 0
  tab2use$marine[tab2use$binomial %in% marinesp$binomial] <- 1 #sets any species in the marine table as marine in the species table
  
  tab2use$marine[tab2use$Group=="Coral"] <- 1 #set all corals to marine
    
  unique <- unique(tab2use) #remove any duplicate species records
    
  nrow(unique(tab2use[tab2use$marine == 1,])) #number of marine species
  
  head(tab2use)

  species <- unique(tab2use$binomial)
  length(species) 
  
  
  ## Part 4 - Run species-country overlaps
  #create dataframes for outputs
  final1 <- data.frame() #create df for country overlaps with species polygon maps
  taberrors <- data.frame() #create df for species-country overlap errors
  marine1 <- data.frame() #create df for species-eez (marine) overlaps
  finrasters <- data.frame() #create df for country overlaps with species raster maps
  
  for (x in 1:length(species)){
    #x <- 1
    print(paste(x, "in", length(species)))
    
    ## Part 4.1 data set-up for species-country overlap
     
    spr <- tab2use[x ,]
    sp <- spr$binomial
    sp
    #marine=T
    marine <- F
    if (spr$marine == 1){
      marine <- T
    }
    marine
    
    spr$size <- (as.numeric(st_area(spr))/1000000) #calculate area of species' range, divided by 1million to convert m2 to km2
    
    shp <- spr
    shp
    
    str(shp)
    #shp
    
    #shp <- shp[shp$presence%in%c(1, 2, 4, 5),] #this should already have been filted and dissolved prior to input
    #shp <- shp[shp$origin%in%c(1, 2, 6),]
    #str(shp)

    nrow(shp)
  
    if (nrow(shp) > 0){  ### some shapes result in empty after subselecting by origin (e.g. mammal Solomys salamonis)
      areaspdeg <- NULL

  ###overlap using the vector layer (for species with smaller ranges - less than 100,000km2)
  
      #if (spr$size<1000)
      
      #Size determines whether to use species polygons or rasters
      if (spr$size < 1000){ #run polygon overlap if species range is less than 1,000km2
        
        # if (! "sf" %in% class(cntsw)) {
        #   cntsw <- as(cntsw, Class = 'sf') #convert into sf objects for polygon overlap code to work
        # }
        
        ## Part 4.2 Species polygon overlap with countries
        if (nrow(shp) > 1){ #if more than one polygon in the shape file, combine into one, this loop should account for 2+ polygons for a species, i.e. presence, origin season combinations - note these should have been filtered prior to running the analyses
          shp2 <- NULL
          shp$id2 <- 1
          shp2 <- tryCatch({st_union(shp)}, error = function(e){})
          if (nrow(shp2) > 0){
            shp <- shp2
            } #if shp2 exists, set shp to shp2 (i.e. the combined polygon)
          }
        if (nrow(shp) == 1){ #only one polygon per species (which should be the case as should have had all ranges dissolved prior to analyses)
          map2 <- F
          if (map2){
            win.graph(6,6)
            plot(shp, border = 6, lwd = 3)
            map("world", add = T)
          }
          finalint <- data.frame()
          a <- cntab
          a$spp <- 0
          a$endemic <- 0
          a$marine <- spr$marine
          a$overlpdegree <- 0
          a$areaspdeg <- NA
          areaspdeg <- suppressWarnings(tryCatch({as.numeric(st_area(shp, byid = FALSE))}, error = function(e){}))
          if (length(areaspdeg) > 0){
            a$areaspdeg <- areaspdeg
          }
          inters <- tryCatch({st_intersects(cntsw, shp, sparse = F)}, error = function(e){}) #which countries intersect with the species
          if (length(inters) > 0){
            a$inters <- inters
          }
          if (length(inters) == 0){
            a$inters <- F
          }
          head(a)
          
          length(which(a$inters == T)) #check how many countries a species overlaps with
        
          if (length(which(a$inters == T)) == 1 & marine == F){    ## identifies endemic species - if only overlap with one country (hence length of overlap is 1)
            aa <- a[a$inters == T,]
            aa$spp <- 1
            aa$endemic <- 1
            finalint <- rbind(finalint, aa[,c("ISO3","Areakm","id","spp","endemic","marine","overlpdegree","areaspdeg")])
            }
      
          
          
          if (length(which(a$inters == T)) > 1 | (length(which(a$inters == T)) > 0 & marine == T)){ #if a species occurs in 2 or more countries, or is a marine species
          #if (length(which(a$inters == T)) > 0 | (length(which(a$inters == T)) > 0 & marine == T)){ #to include endemic species
            
            a$totover <- t(st_contains(shp, cntsw, sparse = F)) #checks if species range within countries 
            #a$totover2 <- st_covers(cntsw, shp, sparse = T)
            
            a$spp[a$inters == T & a$totover == T] <- 1   ## species completely within a country
            ctscov <- a$id[a$spp == 1]
            if (length(ctscov) > 0){
              for (w in 1:length(ctscov)){
                id2 <- ctscov[w]
                a$overlpdegree[a$id == id2] <- suppressWarnings(st_area(cntsw[id2,]))
                }
              }
      
            a[a$inters == T & a$totover == F,]
      
            cts2cut <- a$id[a$inters == T & a$totover == F]    ## countries that overlap with sp but not completely
            length(cts2cut)
      
            if (length(cts2cut) > 0){   #### to account for polygons not overlapping with any country (see coturnix, second polygons season 1)
              for (y in 1:length(cts2cut)){ #looping through each year of overlap
                  #y=1
                  id1 <- cts2cut[y]
                  cnt1 <- cntsw[id1,]
                  cnt1
                  if (map2) plot(cnt1, add = T, col = "lightgrey")
                  
                  overlp = NULL
                  overlp = tryCatch({st_intersection(shp,cnt1)}, error = function(e){})
                  
                  if (map2){
                    plot(overlp, add=T, col=2)
                  }
                  
                  if (nrow(overlp) == 0){ #if no overlaps, set the overlap value to NA  ##note this was throwing an error but I decided to just rasterise all values anyway and so commented this out
                    pp <- NA
                  }              
                  
                  if (nrow(overlp) > 0){ #if there is an overlap, calculate the proportional overlap
                    pp <- suppressWarnings(as.numeric(st_area(overlp))/as.numeric(st_area(cnt1)))
                  }
    
                  a$spp[a$id == id1] <- pp #set the proportional overlap to be either NA (if threw an error) or the calculated proportional overlap
                  
                  if (nrow(overlp) > 0){
                    a$overlpdegree[a$id == id1] <- suppressWarnings(as.numeric(st_area(overlp))) #add in area of overlap to dataframe
                  }
                } #ends loop countries
              } # ends loop when length(cts2cut) > 0
      
            aa <- a[a$inters == T,]
            
            finalint <- rbind(finalint, aa[,c("ISO3","Areakm","id","spp","endemic","marine","overlpdegree","areaspdeg")])
            #finalint <- rbind(finalint, aa[,c("ISO3","id","spp","overlpdegree","areaspdeg")])
            } # ends loop for non endemic species intersecting with a country i.e. length(which(a$inters==T))>1 and marine species
            
          if (length(which(a$inters == T)) > 0){
              finalint$Scientific <- sp
              finalint$TaxonID <- spr$id_no
              #finalint$ordem <- spr$ordem
              finalint$x <- x
              finalint$batch <- tabsp
    
              final1 <- rbind(final1, finalint)
    
              savesp <- T
              if (savesp){
                tname4 <- paste(paste(folder, finfolder2, sep = "/"), "/", x, "_", sp, ".csv", sep="")         #shapesc  ## CHANGE
                tname4
                #tname4=paste(paste(shapesc, "files batches",finfolder,"spbysp/",sep="/"),x, "_",sp, ".csv", sep="")         #
                write.csv(finalint, tname4, row.names = F)
              }
            }
          } # end loop for length(shp)==1 , i.e., no errors when dissolving seasons
        # if (nrow(shp)>1)
        #   {
        #   }
    
        if (length(which(a$inters == T)) == 0) #if not overlap with any countries (an error)
          {
          if (length(areaspdeg) > 0){
            taberrors <- rbind(taberrors, data.frame(Scientific = sp, TaxonID = spr$id_no, error="no_ovelp_with_countries")) 
          }
          tname3 <- paste(paste(folder, finfolder2, sep="/"), "/taberrors_until_", x, ".csv", sep="")       #shapesc  ## CHANGE
          write.csv(taberrors, tname3, row.names=F)
          }
        } # end loop size < 1,000; does not run polygon overlay if run this
    
    ###### rasterize to overlap with countries - for large/complex geometries or when topologic errors occurred with vector calculations
    
      ## Part 4.3 species raster overlap with countries
      
      if (spr$size >= 1000 | nrow(shp) > 1 | length(areaspdeg) == 0){
      #if (nrow(shp) > 0){
      #if (length(shp)>1|length(areaspdeg)==0)
        d <- cntab
        d$endemic <- spr$endemic
        d$marine <- spr$marine
        
        
        
        if (! "SpatialPolygonsDataFrame" %in% class(shp)) {
          shp <- as(shp, Class='Spatial') #convert into sp objects for rasterisation to work
        }
        
        cntsw2 <- as(cntsw, Class='Spatial') #create an sp version of the country layer for raster code to work
        
        # shp <- as(shp, Class='Spatial') #convert into sp objects for rasterisation to work
        # cntsw2 <- as(cntsw2, Class='Spatial')
        
        UDbbox <- bbox(shp) #creates bounding box for raster
        #UDbbox <- as.matrix(data.frame(c(UDbbox[[1]], UDbbox[[2]]), c(UDbbox[[3]], UDbbox[[4]]))) #convert to matrix
        maxrange <- max(diff(range(UDbbox[1,])),diff(range(UDbbox[2,]))) #take largest diff between min and max dimensions (in degrees)
        maxrange
        Res <- 0.5
        maxrange
        
        ## define grid cell of raster depending on range
        if (maxrange < 100){
          Res <- 0.3
        }
        if (maxrange < 50){
          Res <- 0.1
        }
        if (maxrange < 5){
          Res <- 0.05
        }
        Res
        
        BL <- floor(UDbbox[,1]) + (Res/2)
        TR <- ceiling(UDbbox[,2])
        
        maskr <- raster::raster(xmn = BL[1], ymn = BL[2], xmx = TR[1], ymx = TR[2], crs = CRS(proj4string(shp)), resolution=Res, vals=1)
        #create raster of species
        shpr <- shp2raster(shp, maskr, value = 1, label = "shpr", map = F)   #map =F
        map3 <- F
        if(map3){
          plot(shpr)
          plot(cntsw2, add = T)
        }
        
        #do raster country overlaps
        cntr1 <- raster::extract(shpr, cntsw2, cellnumbers = TRUE)
        tpix <- sum(na.omit(getValues(shpr)))
        tpix
        cntr2 <- unlist(lapply(cntr1, function(x) if (!is.null(x)) sum(x[,2], na.rm = TRUE) else 0 ))
        
        d$ppcntraster <- cntr2/tpix
        
        dd <- d[d$ppcntraster > 0,]
        if (nrow(dd) > 0){
          dd$Scientific = spr$binomial
          dd$TaxonID = spr$id_no
          #dd$ordem = spr$ordem
          dd$x <- x
          dd$batch <- finfolder
          sum(dd$ppcntraster)
          finrasters <- rbind(finrasters, dd)
        }
        dd
      
        tname6 <- paste(paste(folder, finfolder2, sep = "/"), "/", x, "_", sp, "_country_raster_.csv", sep = "")      #shapesc  CHANGE
        tname6
        
        write.csv(dd, tname6, row.names=F)        
      
        }
    ######################
    
    ## Part 4.4. marine species raster overlap with EEZs
      if (marine == T){
        b = eeztab
        b$endemic <- spr$endemic
        b$marine <- spr$marine
        
        if (! "SpatialPolygonsDataFrame" %in% class(shp)) {
          shp <- as(shp, Class='Spatial') #convert into sp objects for rasterisation to work
        }
        
        eezw2 <- as(eezw, Class='Spatial') #convert eez layer into spatial for raster code to work
        
        
        UDbbox <- bbox(shp)
        #UDbbox <- as.matrix(data.frame(c(UDbbox[[1]], UDbbox[[2]]), c(UDbbox[[3]], UDbbox[[4]]))) #convert to matrix
        maxrange=max(diff(range(UDbbox[1,])),diff(range(UDbbox[2,])))
        maxrange
        Res=0.5
        maxrange
        if (maxrange<100) Res=0.3
        if (maxrange<50) Res=0.1
        Res
        
        BL <- floor(UDbbox[,1]) + (Res/2)
        TR <- ceiling(UDbbox[,2])
        
  
        maskr <- raster::raster(xmn=BL[1], ymn=BL[2], xmx=TR[1], ymx=TR[2], crs = CRS(proj4string(shp)), resolution=Res, vals=1)
        shpr <- shp2raster(shp, maskr, value = 1, label = "shpr", map = F)
        map4 <- F
        if(map4){
        plot(shpr)
        plot(eezw2, add=T)
        }
    
      eez1 <- raster::extract(shpr, eezw2, cellnumbers=TRUE)
      tpix <- sum(na.omit(getValues(shpr)))
      tpix
      eez2 <- unlist(lapply(eez1, function(x) if (!is.null(x)) sum(x[,2], na.rm = TRUE) else 0 ))
      
      b$ppeez <- eez2/tpix
      
      bb <- b[b$ppeez > 0,]
      if (nrow(bb) > 0)
        {
        bb$Scientific = spr$binomial
        bb$TaxonID = spr$id_no
        #bb$ordem=spr$ordem
        bb$x <- x
        bb$batch <- finfolder
        marine1 <- rbind(marine1, bb)
        }
    
      tname5 <- paste(paste(folder, finfolder2, sep="/"),"/",x, "_",sp, "_eez_.csv", sep="")       #shapesc  CHANGE
      #tname5=paste(paste(shapesc, "files batches",finfolder,"spbysp/",sep="/"),x, "_",sp, "_eez_.csv", sep="")       #shapesc  CHANGE
      tname5
      write.csv(bb, tname5, row.names=F)
      }
    
    } #ends loop for species with valid shapes after subseting for origin (length(shp)>0)
  } #ends loop species
  
  
  #write output files for each of species polygon-country overlaps, species raster-country overlaps and marine species raster-EEZ overlaps
  fnamef = paste(paste(folder, finfolder,sep="/"),"/final_",trunc(as.numeric(Sys.time())), ".csv", sep="")
  fnamef
  write.csv(final1, fnamef, row.names=F) #produces empty file as didn't overlay species polygons (only ran them as rasters to speed up analyses)
  
  fnamef2 = paste(paste(folder, finfolder,sep="/"),"/final_marine_",trunc(as.numeric(Sys.time())), ".csv", sep="")
  fnamef2
  write.csv(marine1, fnamef2, row.names=F)
  
  fnamef3 = paste(paste(folder, finfolder,sep="/"),"/final_rast_",trunc(as.numeric(Sys.time())), ".csv", sep="") #changed to _rast as _raster is then picked up in later steps so doublecounted
  fnamef3
  write.csv(finrasters, fnamef3, row.names=F)

} #end loop through the different batches
  
############################################################################


## Part 5 Merge all species batches for polgon and raster (and EEZ) overlaps

foldersp <- list.files(paste(folder, finfolder, sep="/")) #create a list of all batches run

foldersp <- foldersp[!grepl('.csv', foldersp)] #removing the final output csv files - i.e. only including folders with individual files for species (remove final, final_marine and final_rast)               

tball <- data.frame()
for (g in 1:length(foldersp)){
  #for (g in 1)
  #g=1
  #g=2
  folderf <- paste(folder, finfolder, foldersp[g], sep="/")
  folderf
  print(foldersp[g])
  fics <- dir(folderf, full.names = T)
  fics2 <- dir(folderf, full.names = F)
  length(fics)
  
  #the below creates a table to create a record for each species which details the number of overlaps using polygons or rasters with countries and if they were overlapped with EEZ
  tfile <- data.frame(ficc = fics, fic = fics2, resvect = 0, resraster = 0, rezeez = 0)
  
  for (x in 1:nrow(tfile)){
    tfile$resraster[x] <- length(strsplit(fics2[x], "_raster")[[1]]) - 1 #number of countries the species raster map overlapped with
  }
  
  for (x in 1:nrow(tfile)){
    tfile$rezeez[x] <- length(strsplit(fics2[x], "_eez")[[1]]) - 1 #number of EEZs the species raster map overlapped with
  }
  
  tfile$resvect[tfile$resraster == 0 & tfile$rezeez == 0] <- 1 #number of countries the species polygon map overlapped with (either used raster or eez, not both, so mutually exclusive)
  
  
  head(tfile[,2:5])
  head(tfile)
  nrow(tfile)
  sum(tfile$resvect)
  sum(tfile$resraster)
  sum(tfile$rezeez)
  
  ########################################################################
  #### results vector analyses
  
  tabvect <- data.frame()
  #if (sum(tfile$resvect)>0)
  if (sum(tfile$resvect) > 1){
  
    ficsf <- as.character(tfile$ficc[tfile$resvect==1])
    ficsf2 <- as.character(tfile$fic[tfile$resvect==1])
    
    length(ficsf)
    
    for (z in 1:length(ficsf)){
      #z=8
      #z=which(ficsf2==paste(704,"Treron griveaudi.csv", sep="_"))
      #z=1
      tabs <- read.csv(ficsf[z])
      tabs
      tabsf <- NULL
      
      # tabs$pp <- tabs$overlpdegree/tabs$areaspdeg
      # tabsf <- tabs[,c("Scientific","TaxonID","marine","ISO3","pp")]
      # tabsf$ovl <- "terrestrial"
      # tabsf$method <- "vector_overlap"
      # tabvect <- rbind(tabvect,tabsf)

      if (tabs$marine[1] == 1){ #if species is marine
        tabs$pp <- tabs$overlpdegree/tabs$areaspdeg
        tabsf <- tabs[,c("Scientific","TaxonID","marine","ISO3","pp")]
        tabsf$ovl <- "terrestrial"
        tabsf$method <- "vector_overlap"
        tabvect <- rbind(tabvect,tabsf)
      }
      tabsf

      #overlap of non-marine and endemic species
      if (tabs$marine[1] == 0 & tabs$endemic[1] == 1){
        tabs$pp <- 1 #overlap is 100% with one country for endemic species
        tabsf <- tabs[,c("Scientific","TaxonID","marine","ISO3","pp")]
        #tabsf <- tabs[,c("Scientific","id","ISO3","pp")] #replace TaxonID (lose this) with an ID column (not taxonID)
        tabsf$ovl <- "terrestrial"
        tabsf$method <- "vector_overlap"
        tabvect <- rbind(tabvect,tabsf)
      }
      tabsf
      
      #overlap of non-marine and non-endemic species
      if (tabs$marine[1] == 0 & tabs$endemic[1] == 0){
        tabs$areaspkm <- tabs$Areakm*tabs$spp
        Sumareakm <- sum(tabs$areaspkm)
        tabs$pp <- tabs$areaspkm/Sumareakm
        tabsf <- tabs[,c("Scientific","TaxonID","marine","ISO3","pp")]
        tabsf$ovl <- "terrestrial"
        tabsf$method <- "vector_overlap"
        tabvect <- rbind(tabvect,tabsf)
      }
      tabsf

    }
    
  head(tabvect)
  lu(tabvect$Scientific)
  
  }
  
  ########################################################################
  ####  results raster analyses
  tabrast <- data.frame()
  if (sum(tfile$resraster) > 0){
    ficsf <- as.character(tfile$ficc[tfile$resraster == 1])
    ficsf2 <- as.character(tfile$fic[tfile$resraster == 1])
    
    length(ficsf)
    
    for (z in 1:length(ficsf)){
      #z=1
      #z=which(ficsf2==paste(704,"Treron griveaudi.csv", sep="_"))
      
      tabs <- read.csv(ficsf[z])
      tabs
      tabsf <- NULL
      tabs$pp <- tabs$ppcntraster
      if (nrow(tabs) > 0){
        tabsf = tabs[,c("Scientific","TaxonID","marine","ISO3","pp")]
        #tabsf <- tabs[,c("Scientific","id","ISO3","pp")]
        tabsf$ovl <- "terrestrial"
        tabsf$method <- "raster_overlap"
        tabrast <- rbind(tabrast,tabsf)
        tabsf
      }
    }
    head(tabrast)
  }
  ########################################################################
  ####  results eez analyses
  tabeez <- data.frame()
  if (sum(tfile$rezeez) > 0){
    
    ficsf <- as.character(tfile$ficc[tfile$rezeez == 1])
    ficsf2 <- as.character(tfile$fic[tfile$rezeez == 1])
    
    length(ficsf)
    
    for (z in 1:length(ficsf)){
      #z=1
      #z=which(ficsf2==paste(704,"Treron griveaudi.csv", sep="_"))
      
      tabs <- read.csv(ficsf[z])
      tabs
      tabsf <- NULL
      tabs$pp <- tabs$ppeez
      if (nrow(tabs) > 0){
        tabsf=tabs[,c("Scientific","TaxonID","marine","ISO3","pp")]
        #tabsf <- tabs[ ,c("Scientific","id","ISO3","pp")]
        tabsf$ovl <- "eez"
        tabsf$method <- "raster_overlap"
        tabeez <- rbind(tabeez,tabsf)
        tabsf
      }
    }
    head(tabeez)
  }
  
  ####################################################
  ### Part 5 - combine all files
  nrow(tabvect)
  nrow(tabrast)
  nrow(tabeez)
  
  tabf1 <- rbind(tabvect,tabrast)
  tabf <- rbind(tabf1,tabeez)
  print(lu(tabf$Scientific))
  tabf$batch <- foldersp[g]
  
  tabfinal <- tabf
  tabfinal <- unique(tabfinal)
  
  ###### errors - marine species that do not overlap with any eez (missing values of pp; e.g. "Pterodroma caribbaea" )
  
  #2 species that are missing are terrestrial endemics
  # tabfinal$pp[tabfinal$Scientific == 'Mogera uchidai'] <- 1
  # tabfinal$pp[tabfinal$Scientific == 'Peromyscus caniceps'] <- 1
  # tabfinal$ISO3[tabfinal$Scientific == 'Mogera uchidai'] <- 'JPN' #island off coast of japan
  # tabfinal$ISO3[tabfinal$Scientific == 'Peromyscus caniceps'] <- 'MSR' #montserrat
  
  tabfinal <- tabfinal[!is.na(tabfinal$pp),] #step added so you don't try to sum species with NAs (sum of anything with NA equals NA)
  res2 <- with(tabfinal, aggregate(pp, list(Scientific = Scientific), sum))
  max(res2$x)
  spnas <- unique(tabfinal[is.na(tabfinal$pp),]$Scientific)
  spnas
  lu(spnas) #use these spnas (species with NAs for overlap) to input back into the country-species overlay # end 2019 I did this and realised missing species were endemics so added (so not an issue they were missed as I have now re-run these via vector analyses) - the code is now updated to include endemic species
  
  
  
  #tabfinal[tabfinal$Scientific==spnas[3],]
  unique(tabfinal[is.na(tabfinal$pp),]$Scientific)
  unique(tabfinal[is.na(tabfinal$pp),]$method)
  unique(tabfinal[is.na(tabfinal$pp),]$ovl)
  nrow(tabfinal)
  nnas <- nrow(tabfinal[is.na(tabfinal$pp),])
  tabfinal <- tabfinal[!is.na(tabfinal$pp),]
  nrow(tabfinal)
  nrow(tabfinal) + nnas
  
  
  ## Part 6 - check species values and adjust to 1 if above/below as appropriate (keeping same ratio between countries)
  
  ### sp with ovl > 1 (e.g. different geometries for eez and countries)
  res1 <- with(tabfinal, aggregate(pp, list(Scientific = Scientific), sum))
  max(res1$x)
  sest <- res1$Scientific[res1$x > 1]
  lu(sest)
  
  if (lu(sest) > 0){
    for (w in 1:length(sest)){
      #w=1
      dat2 <- tabfinal[tabfinal$Scientific == sest[w],]
      dat2
      ppt <- sum(dat2$pp)
      tabfinal$pp[tabfinal$Scientific == sest[w]] = tabfinal$pp[tabfinal$Scientific == sest[w]]/sum(tabfinal$pp[tabfinal$Scientific == sest[w]])
      sum(tabfinal$pp[tabfinal$Scientific == sest[w]])
    }
    
    #final check
    res2 <- with(tabfinal, aggregate(pp, list(Scientific = Scientific), sum))
    print(max(res2$x))
    
    t1 <- with(tabf[tabf$Scientific%in%sest,], aggregate(pp, list(Scientific = Scientific, ISO3 = ISO3), sum))
    nrow(t1)
    t2 <- with(tabfinal[tabfinal$Scientific%in%sest,], aggregate(pp, list(Scientific = Scientific, ISO3 = ISO3), sum))
    names(t2)[length(t2)] = "ppfixed"
    nrow(t2)
    
    tf <- merge(t1, t2, all.x=T, all.y=T)
    tf$dif <- tf$x-tf$ppfixed
    max(tf$dif)
    print(tf[tf$dif > .1,])
  }
  
  tball <- rbind(tball, tabfinal)
}

lu(tball$Scientific)

fnamef2 <- paste("C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/",trunc(as.numeric(Sys.time())), ".csv", sep = "")  #shapesc  CHANGE
fnamef2
# write.csv(tball, fnamef2, row.names=F)

######################################################### identify missing species
#tball=read.csv("//AZ-FILESERVER2/gis_data/Tracking Ocean Wanderers/IUCN_R_scripts/RLI/final_pp_file_V1.csv")

tabsp <- st_set_geometry(spTr, NULL) #create table of species

lu(tabsp$binomial)
unique(tball$batch)

tres = with(tball, aggregate(pp, list(Scientific=Scientific, marine=marine), sum))
tres$ncountries = with(tball, aggregate(pp, list(Scientific = Scientific, marine=marine), lu))$x

min(tres$x)
max(tres$x)
hist(tres$x)
tres$sumpp <- tres$x
head(tres[tres$sumpp > 2,])

### to see which species are missing in the table (for which we have map)

#tabm = merge(tabsp[,c("Scientific","Group","TaxonID","marine")], tres[,c("Scientific","ncountries","sumpp")], by="Scientific", all.x=T)

tabm <- merge(tabsp[,c( "binomial", "group", "id_no")], tres[,c("Scientific","ncountries","sumpp")], by.x = "binomial", by.y = "Scientific", all.x=T)
nrow(tabsp)
nrow(tabm)
nrow(tabm[is.na(tabm$sumpp),])
misg <- tabm[is.na(tabm$sumpp),]
#unique(misg$marine)
unique(misg$group)

nrow(misg)
head(tball)
nrow(tball)
tail(tball)

# TODO if you have a list of endemic species and want to include this in the analyses, uncomment below:
# head(endemics)
# missing <- endemics[(endemics %in% misg),] #find the species that are missing from the species-country overlap - note if you are only running a sample make sure you only include species that should have been run
# tball2 <- rbind(tball, endemics) #add these missing endemics to the output - note make sure you have matching fields in the same order or else this won't work (i.e Scientific, TaxonID, marine, ISO3, pp (set to 1), ovl (terrestrial), method (endemic), batch (whatever folder it should have been in))
# tball <- tball2

### to check sum of overlap (should be 1 or near 1 for all non-marine species)
nrow(tres[tres$x < .8,])
#min(tres$marine[tres$x<.8]) ### should be 1 (i.e., only marine species)  or extinct species
min(tres$x) ### should be 1 (i.e., only marine species)  or extinct species
tres[tres$x<.8 & tres$marine == 0,] #should be 0
tres[tres$x<.5,]
tres$sumpp <- tres$x

#tball[tball$Scientific=="Arenaria melanocephala",]  ### coastal species  - rescale to sum 1
#tball[tball$Scientific=="Calidris virgata",]    ### coastal species  - rescale to sum 1
#tball[tball$Scientific=="Cycas sphaerica",]     ### endemic species; step below to reset to 1

#### rescale all pps to sum up 1

sp2rescale <- tres$Scientific[tres$x < .999]
lu(sp2rescale)

for (y in 1:length(sp2rescale)){
  #y=1
  sp <- as.character(sp2rescale[y])
  sp
  tball[tball$Scientific == sp,]
  sum(tball$pp[tball$Scientific == sp])
  tball$pp[tball$Scientific == sp] <- tball$pp[tball$Scientific == sp]/sum(tball$pp[tball$Scientific == sp])
  tball[tball$Scientific == sp,]
  sum(tball$pp[tball$Scientific == sp])
}

#tres2 <- with(tball, aggregate(pp, list(Scientific = Scientific, marine = marine), sum))
tres2 <- with(tball, aggregate(pp, list(Scientific = Scientific), sum))
min(tres2$x)
max(tres2$x)

###########################

## Part 7 - write the output of the analyses to file
# save the final file

all <- merge(tball, tabm, by.x='Scientific' , by.y = 'binomial', all.x = T)
all <- select(all, c('Scientific', 'group', 'TaxonID', 'ISO3', 'pp', 'ovl', 'method', 'batch')) #subset and reorder columns to match usual output

write.csv(all, paste("../Sp_cnts_pp_run_", year_run,".csv", sep=""), row.names = F) #write output of analyses to file



############# Optional - if didn't run all species

#Combine the new ran species with the unchanged species output to create a full list of species-country overlaps for the 5 taxonomic groups
new_run <- read_csv(paste("Sp_cnts_pp_run_", year_run, ".csv", sep="")) #read in new species-country intersection output if needed

prev <- read_csv('unchanged_output_since_last_year_updated.csv') #read in last years species country overlap information for species that haven't had changes to their range maps and are still taxonomically recognised
prev <- select(prev, c('Scientific', 'group', 'TaxonID', 'ISO3', 'pp', 'ovl', 'method', 'batch')) #subset to useful columns
unchanged_run <- prev[!(prev$Scientific %in% new_run$Scientific),] #subset to species that were not re-ran from last years analyses to carry over

overlap_final <- rbind(new_run, unchanged_run) #combine this years run overlaps with last years overlap information that hasn't changed

overlap_final$group <- tolower(overlap_final$group) #set all taxonomic group names to lower case as some upper and some lower case for first letter

write.csv(overlap_final, paste("../Sp_cnts_pp_FINAL_", year_run, "_all.csv", sep=""), row.names = F) #write output for RLI analyses including last years 
