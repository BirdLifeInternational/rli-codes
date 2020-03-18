## Code to subset to species to run for the country-species overlap for RLI that year

### packages to instal
kpacks <- c('tidyverse', 'sf')  ### install automatically the relevant packages
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

### files to import 
folder="C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/input files country intersection" ## copy paste here the working folder - where the csv input files are stored
setwd(folder)

shapesc <- 'C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/shapefiles_RLI'   # folder with the shapefiles with countries and EEZs  (copy-paste the path if different from the one defined in "folder")


##TODO download mammal and amphibian range maps from IUCN website, get bird range maps from shared drive and if need to update cycad and coral data, get this from IUCN (have to request it from Ackbar/Craig)
##TODO need to download species RL categories for birds as these aren't in the range maps from Science folder

## subset to species that need to be run; those newly recognised, newly discovered and those that have had changes to range maps

#read in the layers for each taxon
birds <- st_read(dsn = paste(shapesc, '/Birds_2018.gdb', sep = ''), layer = 'SpeciesRedList_2018_1', stringsAsFactors = F)
mammals <- st_read(dsn = shapesc, layer = 'MAMMALS', stringsAsFactors = F)
amphibians <- st_read(dsn = shapesc, layer = 'AMPHIBIANS', stringsAsFactors = F)

#filter to correct presence, origin, seasonaility combinations
birds2 <- birds %>%
  filter(PRESENCE %in% c(1,2,4,5)) %>%
  filter(ORIGIN %in% c(1,2)) #TODO add in  6
birds2 <- birds2[,c(1:2, 5:7, 15:16)] #subset to useful columns
colnames(birds2) <- c('id_no', 'binomial', 'presence', 'origin', 'seasonal', 'version', 'category', 'Shape')

mammals2 <- mammals %>%
  filter(presence %in% c(1,2,4,5)) %>%
  filter(origin %in% c(1,2))
mammals2 <- mammals2[, c(1:5, 7, 22)]

amphibians2 <- amphibians %>%
  filter(presence %in% c(1,2,4,5)) %>%
  filter(origin %in% c(1,2))
amphibians2 <- amphibians2[, c(1:5, 7, 22)]

# add group to identify which taxonomic group each species is within
birds2$group <- 'bird'
mammals2$group <- 'mammal'
amphibians2$group <- 'amphibian'

#add RL category into bird data
bird_cat <- read_csv('BL_Category_Criteria_Rationale_2018.csv') #TODO need to update this with a list of all species from the same year with RL code
birds_cat <- select(bird_cat, 'SIS ID', 'Scientific name', '2018 IUCN Red List Category', 'Assessment publication year')
birds2 <- merge(birds2, birds_cat, by.x=c('id_no', 'binomial'), by.y=c('SIS ID','Scientific name'), all.x=T)
birds2$category <- birds2$`2018 IUCN Red List Category`
birds2 <- select(birds2, -c('2018 IUCN Red List Category'))
colnames(birds2)[colnames(birds2) == 'Assessment publication year'] <- 'yrcompiled'

birds3 <- select(birds2, c('id_no', 'binomial'))
mammals3 <- select(mammals2, c('id_no', 'binomial'))
amphibians3 <- select(amphibians2, c('id_no', 'binomial'))

#drop geometry column for filtering purposes
birdsNG <- st_set_geometry(birds2, NULL)
mammalsNG <- st_set_geometry(mammals2, NULL)
amphibiansNG <- st_set_geometry(amphibians2, NULL)

birdsNG2 <- select(birdsNG, -c('version')) #drop version column for complete list
birdsNG2 <- birdsNG2[,c(1:5,8,6:7)] #reorder columns to match mammal and amphibian output
all_species <- rbind(birdsNG2, mammalsNG, amphibiansNG) #combine all species for a full species list output

all_species <- select(all_species, -c('presence', 'origin', 'seasonal')) #remove these columns as don't need for a full species list

all_species <- all_species %>% #filter to get a list of unique species where the year assessed is the maximum year of any assessment
  group_by(id_no, binomial) %>% 
  top_n(1, yrcompiled)

#all_species <- select(all_species, 'id_no', 'binomial', 'yrcompiled', 'category', 'group') #remove presence, origin species columns as just want list of all species
all_species <- unique(all_species) #removes duplicate records so only have one record per species
write.csv(all_species, 'full_species_list.csv', row.names = F)

#identify newly recognised species to run
lstYr <- read_csv('Sp_cnts_pp_FINAL_2018_ANT.csv') #read in last year's species-country overlap output
lstYr <- select(lstYr, -c('X1'))
lstYr <- lstYr %>%
  group_by(Scientific, group, TaxonID, marine, ISO3, ovl, method, batch) %>% 
  summarise(pp = sum(pp)) #where duplicates have been scaled to one, sum duplicates to gives true proportion still
lstYr <- lstYr[c(1:5,9,6:8)] #reorder columns to match original structure


mammalsN <- mammalsNG[!(mammalsNG$binomial %in% lstYr$Scientific),] #new mammals to run
mammalsO <- mammalsNG[(mammalsNG$binomial %in% lstYr$Scientific),] #old mammals to keep

birdsN <- birdsNG[!(birdsNG$binomial %in% lstYr$Scientific),] #new birds to run
birdsN <- select(birdsN, -c('version'))
birdsN <- birdsN[,c(1:5, 8, 6:7)] #reorder columns to match other taxa
birdsO <- birdsNG[(birdsNG$binomial %in% lstYr$Scientific),] #old birds to keep

amphibiansN <- amphibiansNG[!(amphibiansNG$binomial %in% lstYr$Scientific),] #new amphibians to run
amphibiansO <- amphibiansNG[(amphibiansNG$binomial %in% lstYr$Scientific),] #old amphibians to keep

#identify species with changes to range maps (so need to be re-run)
mammalsR <- mammalsO[mammalsO$yrcompiled > 2016,] #TODO set year to be greater than when last run; subsets to species that have been edited since the overlap was last run
mammalsL <- mammalsO[mammalsO$yrcompiled <= 2016,] #TODO set year to be less than or equal to last year run; species that don't need to be rerun and can go straight into output

#birdsO$yrcompiled <- as.numeric(birdsO$yrcompiled) #set version column to be numeric

#birdsO$version <- as.numeric(birdsO$version)
birdsR <- birdsO[birdsO$version > 2018,] #TODO set year to be greater than when last run; subsets to species that have been edited since the overlap was last run ##TODO change this to factor in all ranges where only one may have changed
birdsR <- select(birdsR, -c('version')) #remove version column once used
birdsR <- birdsR[,c(1:5, 8, 6:7)] #reorder columns to match other taxa
birdsL <- birdsO[birdsO$version <= 2018,] #TODO set year to be less than or equal to last year run; species that don't need to be rerun and can go straight into output
birdsL <- select(birdsL, -c('version'))
birdsL <- birdsL[,c(1:5, 8, 6:7)] #reorder columns to match other taxa


amphibiansR <- amphibiansO[amphibiansO$yrcompiled > 2016,] #TODO set year to be greater than when last run; subsets species that have been edited since the overlap was last run
amphibiansL <- amphibiansO[amphibiansO$yrcompiled <= 2016,] #TODO set year to be less than or equal to last year run; species that don't need to be rerun and can go straight into output

#combine lists of all species to run this year
sp_tr <- rbind(mammalsN, mammalsR, birdsN, birdsR, amphibiansN, amphibiansR)
sp_tr <- unique(select(sp_tr, -c('presence', 'origin', 'seasonal'))) #subset columns to just species SISID and scientific name, so unique row for each species
sp_tr <- sp_tr %>% #filter to get a list of unique species where the year assessed is the maximum year of any assessment (ensuring most recent assessment and RL category)
  group_by(id_no, binomial) %>% 
  top_n(1, yrcompiled)


thisYr <- lstYr[lstYr$Scientific %in% mammalsL$binomial | lstYr$Scientific %in% birdsL$binomial | lstYr$Scientific %in% amphibiansL$binomial,] #creates output of species that don't need changing and so the output is the same as last year, needs to be combined at the end

thisYr2 <- thisYr[!(thisYr$Scientific %in% sp_tr$binomial),] #creates output of species that don't need changing and so the output is the same as last year, needs to be combined at the end

write.csv(thisYr2, 'unchanged_output_since_last_year_updated.csv', row.names = F) #outputs csv file of country-species overlaps for unchanged species


#for species that need to be re-run, join with geometry
birdsG <- merge(birds3, sp_tr, by=c('id_no','binomial'))
#birdsG <- select(birdsG, -c('version'))
#birdsG <- birdsG[, c(1:5, 8, 6:7, 9)] #reorder columns to match other taxa

mammalsG <- merge(mammals3, sp_tr, by=c('id_no','binomial'))
amphibiansG <- merge(amphibians3, sp_tr, by=c('id_no', 'binomial'))


#union all species information so only have one record per species
allG <- rbind(birdsG, mammalsG, amphibiansG)

allG <- select(allG, 'id_no', 'binomial', 'yrcompiled', 'category', 'group', 'geometry')

# allG2 <- allGt %>%
#           group_by(binomial) %>%
#           st_union()

st_write(allG, dsn = paste(shapesc,'/species_to_run_RLI_2019_noPOS', sep=''), driver = 'ESRI Shapefile', update = T) #TODO change year to year of run

##############################

#create marine file to filter species who need to be overlaid with EEZs

### packages to instal
kpacks <- c('tidyverse', 'sf')  ### install automatically the relevant packages
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

### files to import 
folder="C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/input files country intersection" ## copy paste here the working folder - where the csv input files are stored
setwd(folder)

shapesc <- 'C:/Users/Ashley.Simkins/Documents/SDG/RLI/RLI_revised_2019/shapefiles_RLI'   # folder with the shapefiles with countries and EEZs  (copy-paste the path if different from the one defined in "folder")

mammals <- st_read(dsn = shapesc, layer = 'MAMMALS', stringsAsFactors = F) #read in mammal information
mammals <- st_set_geometry(mammals, NULL) #remove geometry
mammals <- select(mammals, c('id_no', 'binomial', 'category', 'marine')) #subset to relevant columns
#colnames(mammals)[colnames(mammals)=='terrestial'] <- 'terrestrial' #rename terrestrial column with correct spelling

birds <- read_csv('birds_marine.csv') #read in bird species information (on whether use marine ecosystems or not)
birds <- select(birds, -c('terrestrial', 'freshwater'))
birds$marine[birds$marine=='y'] <- TRUE
birds$marine[is.na(birds$marine)] <- FALSE

all <- rbind(birds, mammals) #combine bird and mammal lists together
all <- unique(all) #remove any duplicates

marine <- all[all$marine == TRUE,] #subset to species that use marine habitats
marine <- unique(marine) #remove any duplicates
write.csv(marine, 'marine.csv', row.names = FALSE) #write output into directory





