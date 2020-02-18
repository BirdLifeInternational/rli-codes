
# General and Disaggregated RLIs calculator 
# Version 2.0 12/02/2020
# Maria Dias/BirdLife International; maria.dias@birdlife.org
# Script to calculate and plot the Global, Regional and Disaggregated RLIs
## See formats of input tables in word document "Code to calculate general and disaggregated Red List Indices (RLIs) Version 2.0. Instructions and recommendations"


# PART 1. SET PARAMETERS AND INDICATE FILE NAMES AND FOLDER LOCATIONS   ###############################
########################################################################################################

# 1.1. SET FOLDER LOCATIONS AND NAMES OF FILES ############################################################

wkfolder="C:/Users/IUCN R codes/2020/input files"      ## copy-paste the working folder, replacing "/" by "/" and ensuring no spaces before & after quotes
setwd(wkfolder)

finfolderP="C:/Users/IUCN R codes/2020/outputs_RLI"   ##  copy-paste the location of the folder where the country files should be saved, replacing "/" by "/" and ensuring no spaces before & after quotes

isos=read.csv("Iso_countries_2019.csv")         ## file with ISO codes; should be stored in the wkfolder specified above; should have the following columns (with these exact names): "ISO3","ISO_BL","ISO_SDG","LDC","LLDC_SIDS","IPBES_region","SDG_Region","SDG_Subregion","CMS_Region"
tabsp1=read.csv("full_species_list.csv")   ## file with list of species; should be stored in the wkfolder specified above; should have the follwoing columns (with these exact names): "TaxonID","Scientific","Group","Cat_latest"; Scientific= Latin name; Group= taxonomic group (Amphibian, Bird, Coral, Cycad or Mammal); Cat_latest= latest IUCN red list category - STANDARTIZED!! ONLY POSSIBLE OPTIONS ARE "LC","NT","VU","EN","CR","CR(PE)","CR(PEW)","EW","EX","DD"
gench1=read.csv("genuine_changes_2019_drivers.csv")     ### file with genuine changes; should be stored in the wkfolder specified above  #make sure no files in the genuine changes go past the RL completion cycle (e.g. 2016-2020); should have the follwoing columns (with these exact names): "TaxonID","Scientific","Group","Cat_latest"
sppp=read.csv("Sp_cnts_pp_FINAL_2019_new_corrected.csv")  ## result of the spatial analyses (see Annex 1) to estimate the proportion of the range maps in each country;  should be stored in the wkfolder specified above; should have the follwoing columns (with these exact names): "TaxonID","Scientific","ISO3","pp"
gcc=read.csv("RLI_gc_codes.csv")    ### table with the groups and codes for the genuine changes table (with years of assessment)
codesreg=read.csv("codings_sdg_template.csv")  # table with the codes for UN reporting; only needed if to save fowlloing their templates (see below "save_UN_template")
#other_groups=read.csv("Other_species_lists.csv") # table with the taxonIDs of other groups of species to run; only needed if to run disaggregated RLIs for used-defined groups of species (see below option other_species_lists=T)


folder_mammals="Mammalia_2019-3"
folder_birds1="Aves_Part1_Non_Passeriformes_2019-3"
folder_birds2="Aves_Part2_Passeriformes_2019-3"
folder_amphibia="Amphibia_2019-3"
folder_corals="Reef_forming_Corals_2019-3"
folder_cycads="Cycads_2019-3"

# 1.2. SET BASIC PARAMETERS FOR THE ANALYSES

maxyear=2020      ### max year of the analyses - change to new max year (was 2017)
repetitions=1000     ## n repetitions - change to new max rep (was 1000, reduced to reduce processing time)
saveglobal=T           ### set as FALSE if no need to save global rlis (i.e., only to save the subglobal)
skip_subglobal=F           ### set as TRUE if no need to run subglobal rlis (i.e., only to run the global)
skip_thematic=F     # if only to run general RLIs (i.e., skip the thematic RLI)
only_thematic=F     # if only to run thematic RLIs (i.e., ignore the general RLI)
dis_subglobal=T     # if disaggregated RLIs to be calculated also at subglobal levels
save_UN_template=F # to save the results in the UN template 
other_species_lists=F # if to run RLIs for user-defined groups of species (i.e., not the standard derived from IUCN table)
pdfs=T            ### if plots to be saved as pdf files
pngs=T             ### if plots to be saved as png files
slopecv=.6       ### standard deviation of the slope
name_UN_template="Table_RLI_2020_Jan2020.csv"

# 1.3 SET THE DISAGGREGATIONS TO RUN

#######  1.3.1. based on isocodes or combinations of isocodes - check match between names and columns in isos table

isos2use=c("ISO_BL","ISO_SDG","SDG_Region","SDG_Subregion","LLDC_SIDS","LDC","IPBES_region","CMS_Region")#,"SDG_Subregion","IPBES_region"  ### IMPORTANT NOTE!!! IF TO RUN BOTH ISO_BL AND ISO_SDG, ISO_BL SHOULD BE FIRST IN THE LIST    
 
### add or remove as needed; global will always run by default; check perfect match with column names in isos table; 
##if to run only global, set empty vector: isos2use=NULL

if (skip_subglobal) isos2use=NULL
length(isos2use)

#######  1.3.2.  based on drivers of genuine changes - check match between names in following vectors and columns in gench1 table
### options are: c(utilisation=utilisation,IAS=IAS,pollution=pollution,fisheries=fisheries)                                           f
# IMPORTANT NOTE: leave empty vector if not to run any RLI based on the drivers of genuine changes:  driv_gc=NULL

# set here the fields in the table of the genuine changes corresponding to each driver
utilisation=c("driverHuntingAll", "driverHuntingInternational", "driverLogging", "driverFisheries", "driverPlants") 
IAS="driverInvasiveSpecies"
pollution="driverPollution"
fisheries="driverFisheries"

# set here the drivers to run; repeat names as shown: XXX=XXXX (e.g.  driv_gc=list(utilisation=utilisation,IAS=IAS,pollution=pollution,fisheries=fisheries)
driv_gc=list(utilisation=utilisation,IAS=IAS, fisheries=fisheries,pollution=pollution) #
#driv_gc=NULL
if (skip_thematic) driv_gc=NULL

length(driv_gc)

list_drivers=c("driverAgriculture", "driverLogging","driverFisheries","driverInvasiveSpecies", "driverNativeSpecies",
"driverHuntingAll","driverHuntingInternational","driverResidential","driverEnergy","driverFire","driverDams","driverClimate",
"driverPollution","driverHumanDisturbance","driverTransportation","driverGeological","driverPlants","driverOtherDiseases","driverUnknown")

### 1.3.3.  based on groups of species 
## IMPORTANT NOTE:  leave empty vector if not to run any RLI based on the groups of species:  splists=NULL; if to run user-defined lists of species, set  other_species_lists=T above and check that names below match the names in the table "other_groups"

splists=c("internationallytraded","medicine","forest","wetland","migratory","marine","terrestrial","freshwater")  # internationallytraded	medicine	forest	wetland	migratory	marine	terrestrial	freshwater
if (other_species_lists) splists=unique(other_groups$Group_species)
splists
#splists=NULL
if (skip_thematic) splists=NULL

length(splists)

### 1.3.4.  based on combinations of drivers and groups of species
## n elements in each vector should match; names of vector driv should match the ones defined in 1.3.2
## IMPORTANT NOTE:  leave empty vector if not to run any RLI based on the groups of species:  combs=NULL

combs=data.frame(driv=c("utilisation","fisheries", "pollution", "IAS", "IAS"), spgroup=c("migratory","migratory","migratory","migratory","wetland"))  
#combs=NULL
if (skip_thematic) combs=NULL

if (length(combs)==0) combs=data.frame()
nrow(combs)
combs

n_iter=1+length(driv_gc)+length(splists)+nrow(combs) # number of iterations to run (number of disaggregated RLI + the general)
n_iter

# PART 2. CHECK VALIDITY OF INPUT DATA AND PREPARE TABLES  ###############################
########################################################################################################

#### VERY IMPORTANT - CHECK IF NAMES IN isos2use VECTOR MATCH COLUMN NAMES IN isos TABLE; FOLLOWING RESULT SHOULD BE 0
length(which(isos2use%in%names(isos)==F))  ### should be 0 

isos2use=isos2use[which(isos2use%in%names(isos))]   ## will delete the misspelled names (i.e., not matching with the columns in isos2use)
length(isos2use)


## verify if the number of iso codes in pps sp-countries is the same as in isos 
lu=function (x=x) length(unique(x))   ## function to estimate the number of unique values in a vector
lu(sppp$ISO3) ##number of unique iso codes in table with the pps sp-countries (should be the same or one less - ABNJ - comparing with the isos)
isos$ISO3[which(!isos$isof%in%sppp$ISO3)] # confirm that the difference in isos between tables (isos and sp-countries-pp) is due to the ABNJ
sort(unique(sppp$ISO3[which(!sppp$ISO3%in%isos$ISO3)])) # confirm that the difference in isos between tables (isos and sp-countries-pp) is due to the ABNJ

## prepare table with species' lists 
folders_sp=list(folder_birds1,folder_birds2,folder_mammals, folder_amphibia, folder_corals, folder_cycads)
folders_sp

## traded and medicine
trsp=NULL
medi=NULL
for (a in 1:length(folders_sp))
{
t2read="usetrade.csv"
f2read=folders_sp[[a]]
f2read
dat1=read.csv(paste(f2read,t2read, sep="/"))
head(dat1)
sp2extr1=dat1$internalTaxonId[dat1$international=="true"]
trsp=c(trsp,sp2extr1)
sp2extr2=dat1$internalTaxonId[dat1$code%in%c(1,3)]
medi=c(medi,sp2extr2)
}
tabsp1$internationallytraded=0
tabsp1$internationallytraded[tabsp1$TaxonID%in%trsp]=1
tabsp1$medicine=0
tabsp1$medicine[tabsp1$TaxonID%in%medi]=1

head(tabsp1[tabsp1$internationallytraded==1&tabsp1$Group=="Mammal",])
head(tabsp1[tabsp1$medicine==1&tabsp1$Group=="Mammal",])

# forest and wetlands
forest=NULL
wetland=NULL
for (a in 1:length(folders_sp))
{
#a=3
t2read="habitats.csv"
f2read=folders_sp[[a]]
f2read
dat1=read.csv(paste(f2read,t2read, sep="/"))
dat1$code2=floor(as.numeric(substr(as.character(dat1$code), 1,3)))
#with(dat1, aggregate(code2, list(code=code), max))
first=function (x=x) x[1]
# to identify species with no codes in suitability field
spnocode=with(dat1, aggregate(suitability, list(internalTaxonId=internalTaxonId), lu))
spnocode$first=with(dat1, aggregate(suitability, list(internalTaxonId=internalTaxonId), first))$x
head(spnocode)
unique(spnocode$first)
unique(dat1$suitability)
spnocode_list=spnocode$internalTaxonId[spnocode$x==1&(!spnocode$first%in%c("Suitable","Marginal","Unknown"))]
spnocode_list
if (length(spnocode_list)>0) dat1$suitability[dat1$internalTaxonId%in%spnocode_list]="Suitable" # to simplify, the suitability of all habitats of these non-coded species with be considered "suitable" - so they will be only included if they have no other habitats

# forest
codes2use=c(1,3) # codes for habitat=forest
dat2=dat1[dat1$code2%in%codes2use,] # table with habitat species
unique(dat2$code)
unique(dat2$code2)
habitat1=unique(dat2$internalTaxonId[dat2$majorImportance=="Yes"])  ## list of habitat species for which major importance is true
habitatall=unique(dat2$internalTaxonId) ## list of all habitat species
habitat2=habitatall[which(!habitatall%in%habitat1)]  # habitat species for which major importance is not true
head(dat2[dat2$internalTaxonId%in%habitat2,])
unique(dat2[dat2$internalTaxonId%in%habitat2,]$majorImportance)

dat3=dat1[!dat1$code2%in%codes2use,]#table with non-habitat species
dat3=dat3[dat3$suitability=="Suitable",]
unique(dat3$code)
sort(unique(dat3$code2))

othersp=unique(dat3$internalTaxonId) # species with other suitable habitats  
habitat3=habitat2[which(!habitat2%in%othersp)] # habitat species with no other suitable habitats
head(dat1[dat1$internalTaxonId%in%habitat3,])
habitatf=c(habitat1,habitat3)
forest=c(forest, habitatf)

## wetland
codes2use=c(5,15) # codes for habitat=forest
dat2=dat1[dat1$code2%in%codes2use,] # table with habitat species
unique(dat2$code)
unique(dat2$code2)
habitat1=unique(dat2$internalTaxonId[dat2$majorImportance=="Yes"])  ## list of habitat species for which major importance is true
habitatall=unique(dat2$internalTaxonId) ## list of all habitat species
habitat2=habitatall[which(!habitatall%in%habitat1)]  # habitat species for which major importance is not true
head(dat2[dat2$internalTaxonId%in%habitat2,])
unique(dat2[dat2$internalTaxonId%in%habitat2,]$majorImportance)

dat3=dat1[!dat1$code2%in%codes2use,]#table with non-habitat species
dat3=dat3[dat3$suitability=="Suitable",]
head(dat3)
unique(dat3$code)
sort(unique(dat3$code2))

othersp=unique(dat3$internalTaxonId) # species with other suitable habitats  
habitat3=habitat2[which(!habitat2%in%othersp)] # habitat species with no other suitable habitats
head(dat1[dat1$internalTaxonId%in%habitat3,])
habitatf=c(habitat1,habitat3)
wetland=c(wetland, habitatf)
} # ends loop for forest and wetland species

tabsp1$forest=0
tabsp1$forest[tabsp1$TaxonID%in%forest]=1
tabsp1$wetland=0
tabsp1$wetland[tabsp1$TaxonID%in%wetland]=1

head(tabsp1[tabsp1$forest==1&tabsp1$Group=="Mammal",])
head(tabsp1[tabsp1$wetland==1&tabsp1$Group=="Mammal",])

## migratory
migr=NULL
for (a in 1:length(folders_sp))
{
#a=3
t2read="all_other_fields.csv"
f2read=folders_sp[[a]]
f2read
dat1=read.csv(paste(f2read,t2read, sep="/"))
head(dat1)
sp2extr1=dat1$internalTaxonId[dat1$MovementPatterns.pattern=="Full Migrant"]
head(dat1[dat1$MovementPatterns.pattern=="Full Migrant",])
migr=c(migr,sp2extr1)
}

tabsp1$migratory=0
tabsp1$migratory[tabsp1$TaxonID%in%migr]=1
head(tabsp1[tabsp1$migratory==1&tabsp1$Group=="Mammal",])

### 	marine	terrestrial	freshwater

marine=NULL
terrest=NULL
freshw=NULL

for (a in 1:length(folders_sp))
{
#a=3
t2read="assessments.csv"
f2read=folders_sp[[a]]
f2read
dat1=read.csv(paste(f2read,t2read, sep="/"))
head(dat1)
unique(dat1$systems)
sp2extr1=unique(dat1$internalTaxonId[dat1$systems=="Marine"])
marine=c(marine,sp2extr1)
sp2extr2=unique(dat1$internalTaxonId[dat1$systems=="Terrestrial"])
terrest=c(terrest,sp2extr2)
sp2extr3=unique(dat1$internalTaxonId[dat1$systems=="Freshwater (=Inland waters)"])
freshw=c(freshw,sp2extr3)
} # ends loop marine, terrestr, fresh

tabsp1$marine=0
tabsp1$marine[tabsp1$TaxonID%in%marine]=1
tabsp1$terrestrial=0
tabsp1$terrestrial[tabsp1$TaxonID%in%terrest]=1
tabsp1$freshwater=0
tabsp1$freshwater[tabsp1$TaxonID%in%freshw]=1

head(tabsp1[tabsp1$marine==1&tabsp1$Group=="Mammal",])
head(tabsp1[tabsp1$terrestrial==1&tabsp1$Group=="Mammal",])
head(tabsp1[tabsp1$freshwater==1&tabsp1$Group=="Mammal",])

if (other_species_lists){
othspecies=unique(other_groups$Group_species)
for (a in 1:length(othspecies))
{
#a=1
extr1=othspecies[a]
extr1
sp2extr1=other_groups$TaxonID[other_groups$Group_species==extr1]
tabsp1$newvar=0
tabsp1$newvar[tabsp1$TaxonID%in%sp2extr1]=1
names(tabsp1)[which(names(tabsp1)=="newvar")]=as.character(extr1)
head(tabsp1)
}}


head(tabsp1)


### prepare gen change table
nrow(gench1)
gench2=merge(gench1, gcc) # to add  the group and years of assessment to the table of genuine changes
nrow(gench2)  # should be the same as previous
gench2$cat_start=gench2$periodStartCategory
gench2$cat_end=gench2$periodEndCategory
if(!"TaxonID" %in% names(gench2))  gench2$TaxonID=gench2$taxonid
gench2=merge(gench2, tabsp1[,c("TaxonID","Scientific")]) ## to add the names of the species to the table
head(gench2)
nrow(gench2)  ## if nrow is different from above - species are missing in the full list
unique(gench2$Group)


### identify which entries don't have a primary driver, but have a major secondary

gench3=data.frame(gench2[,which(names(gench2)%in%list_drivers)])
head(gench3)
sp2check_list1=NULL
sp2check_list2=NULL
for (j in 1:length(gench2))  sp2check_list1=c(sp2check_list1,which(gench2[,j]=="Most Important Secondary",)) 
for (j in 1:length(gench2))  sp2check_list2=c(sp2check_list2,which(gench2[,j]=="Primary",)) 
sp2check_list1=sort(unique(sp2check_list1))
sp2check_list2=sort(unique(sp2check_list2))

sp2check=unique(sp2check_list1[which(!sp2check_list1%in%sp2check_list2)]) # entries that don't have any primary driver but have a  "Most Important Secondary"
length(sp2check)
gench3[sp2check,]

genchF=gench2
for (n in 1:length(sp2check)){
for (j in 1:length(gench3))   if (genchF[sp2check[n],which(names(genchF)%in%list_drivers[j])] =="Most Important Secondary")  genchF[sp2check[n],which(names(genchF)%in%list_drivers[j])] ="Primary"  # replace the "Most Important Secondary" to "Primary"
}
genchF[sp2check,]
nrow(genchF)
nrow(gench1)


wts=data.frame(iucn=c("LC","NT","VU","EN","CR","CR(PE)","CR(PEW)","EW","EX","DD"), wg=c(0,1,2,3,4,5,5,5,5,-1))    # table with the "weights" of each iucn class (for the rli calculations) 

### correct possible errors in cat_latest in tabsp1 table
correct_cats=with(genchF, data.frame(TaxonID=TaxonID, cat_end_GC=cat_end,year_Cat=end_year))
comptables=merge(tabsp1,correct_cats)
comptables=merge(comptables, with(wts, data.frame(Cat_latest=iucn, wg_original=wg)))
comptables=merge(comptables, with(wts, data.frame(cat_end_GC=iucn, wg_corrected=wg)))
head(comptables)
comptables$dif=comptables$wg_original-comptables$wg_corrected
comptables=comptables[comptables$dif!=0,]
nrow(comptables)

for (a in 1:nrow(comptables)) tabsp1$Cat_latest[tabsp1$Scientific==comptables$Scientific[a]]=as.character(comptables$cat_end_GC[a])
#write.csv(tabsp1, "tabsp1_corrected.csv", row.names=F)

####### preparation of tables for the analysis
nrow(sppp)   ###   check number of rows in table with the pps sp-countries (sppp)
head(sppp)   ###    check check structure of table with the pps sp-countries


sppp1=merge(sppp,tabsp1[,c("Scientific","Group","Cat_latest")], sort=F)
nrow(sppp1)   ### check if the number of rows of sppp1 remained the same after the merge
head(sppp1)   ### check if the structure of sppp1 remained the same after the merge - but with some new columns

sppp1$iucn=sppp1$Cat_latest



####### ends of table preparation



#################### verify if all IUCN codes are correct
cds1=unique(tbf$iucn)
length(cds1)
length(cds1[cds1%in%wts$iucn])  ## should be the same as previous

cds1=unique(genchF$cat_start)
length(cds1)
length(cds1[cds1%in%wts$iucn])  ## should be the same as previous

cds1=unique(genchF$cat_end)
length(cds1)
length(cds1[cds1%in%wts$iucn])   ## should be the same as previous

#verify if all the scientific names in genuine file are correct and are in in sp-cnts table
sp1=unique(genchF$Scientific)   #tabf$RLC   #gench$cat_start #gench$cat_end
length(sp1)
length(sp1[sp1%in%tbf$Scientific])
sp1[!sp1%in%tbf$Scientific]      ### species that are missing in the sp-cnts table after excluding the DD (but are in the genuine file)

sprich=with(sppp1, aggregate(Scientific, list(group=Group), lu))
names(sprich)[length(sprich)]="rich"
sprich    ### number of species per taxon

years=min(genchF$start_year):maxyear     # years to consider in the analyses: from the first assessment of all until current
years


# PART 3. CUSTOM FUNCTIONS  ###########################################################################
########################################################################################################


first=function (x=x) x[1]             ## function to output the first value of a vector
q95=function (x=x) quantile(x,.95)    ## function to estimate the quantile 95%
q05=function (x=x) quantile(x,.05)    ## function to estimate the quantile 5%
div0=function (x=x, y=y) trunc((x-1)/y)-((x-1)/y)  ## function that outputs 0 if y-1 is divisible by x (aux open new devices in graphics outputs)
cinz=rgb(217/255,217/255,217/255,.4)   ### grey transparent colour for the plots (in rgb)

################ function to calculate global RLI

# nreps=number of repetitions; groups2=list of taxonomic groups to analyse; leftextrapol=if slope should be extrapolated to the left; saveleftslope=if left slope should be saved; plotit=if each iteraction should be plotted
rlicalc=function (nreps=repetitions, groups2=groups2, years=years, leftextrapol=T, saveleftslope=T, plotit=F){

# create table with the desired structure
  rliTot=data.frame(group=rep(rep(groups2, each=length(years)),length(nreps)), year=rep(rep(years, length(groups2)), length(nreps)), nrep=rep(1:nreps,each=length(years)*length(groups2)), rli=NA)
  outpts=list()   # creates empty list to store outputs generated within each iteration of the following loop

  if (saveleftslope) leftslopes=NULL
  for (r in 1:nreps)  ### loop to go through all repetitions defined above
  {
    #r=1
    for (g in  1:length(groups2))     #loop to go through each taxonomic group
    {
      #g=1
      group=as.character(groups2[g])   # selects the group g
      #group="Bird"
      rlig=data.frame()
      group
      tgroup=pastables[pastables$group==group,] ## past table (see below) for group g
      if (altpasttable & ((r/2)-floor(r/2))==0) tgroup=pastables_alternative[pastables_alternative$group==group,]  # alternative past tables, for disaggregated hald of the iterations for rli related to drivers of genuine changes
      nrow(tgroup)
      yearsa=sort(unique(tgroup$year))    # to create vector years of assessment of group g
      yearsa       # years of assessment of group g

      ### years of assessment
      for (y in 1:length(yearsa))   ## loop to go through each year of assessment of group g
      {
        #y=1
        year=yearsa[y]      # to select year y
        year
        tb=tgroup[tgroup$year==year,]  # assessment of group g in year y
        tb=merge(tb,wts)    # merge with table with the weight of each iucn class
        head(tb)    #check structure merged table
        nrow(tb)    #check number of rows of merged table
        dds=tb$Scientific[tb$iucn=="DD"]
        dds
        head(tb[tb$iucn=="DD",])
        allwgs=tb$wg[tb$wg>(-1)]   ## pool of non-DD red list categories for this group
        if (length(allwgs)==0) allwgs=0 ## if all species of this group are DD
        if (length(dds)>0) ddr=sample(allwgs,length(dds))
        #if (length(dds)>0) for (d in 1:length(dds)) tb$wg[tb$Scientific==dds[d]]=sample(allwgs,1)  ## randomly selects a red list category for DD species, from the pool created above
        sumwgs=sum(allwgs)
        if (length(dds)>0) sumwgs=sum(allwgs)+sum(ddr)        
        rli1=1-(sumwgs/(wex*nrow(tb)))   ## calculation of red list index (rli) for year y
        rli1                                 ##red list index (rli) for year of assessment y
        rlig=rbind(rlig, data.frame(group=group,year=year, rli=rli1)) #  saves the rli for group g and year y
      } ## ends years of assessment
      rlig    # rli for all years of assessment of group g

      ### intrapolation and extrapolation   (done within each group)

      dat=rlig
      dat
      #interpolation
      yearsti=min(dat$year): max(dat$year)  # years between the first and last assessment - for interpolation
      irli=approx(dat$year, dat$rli, xout=yearsti, method="linear", rule=2:2)$y   ## linear interpolation of rli values based on existing years of assessment
      irli   # result of the interpolation

      datf=data.frame(year=yearsti, rli=irli)  # table with years of assessment and interpolated years and correspondent rlis for group g
      datf
      #with(datf, plot(year, rli, xlim=c(1975,2018), ylim=c(0.6,1.2)))  # to plot the results

      #extrapolation    # y=a+bx
      #left extrapolation
      yearl=c(min(years)-2, min(years)-1, years[which(years<min(yearsti))])   ## vector with years for left extrapolation: since two years before the year of first assessment (of any group) and the year of first assessment of group g 
      yearl

      x1=sort(dat$year)[1:2]   ### first two years  of assessment
      x1   
      y1=c(dat$rli[dat$year==x1[1]],dat$rli[dat$year==x1[2]])  ### rli in first two years of assessment
      y1    ###rlis
      nv=lm(y1~x1) # linear model of the rate of change in rli (based on first two years of assessment) to estimate slope and intercept for extrapolation
      mnslp <- coef(nv)[2]  # slope of the model
      useslp <- rnorm(1, mnslp, sd=slopecv*abs(mnslp))  # random value of slope based on the slope from the linear model and a potential standard devidation (set in the begining of the code - "slopecv") - changes each repetition
      if (leftextrapol==F) useslp <- meanleftslopes  # for corals we don't estimate left slope the same way, but we use an average of the slopes of the other groups - so we set leftextrapol==F for corals (and run the code after the other groups)
      irlel=y1[1] - (useslp*(x1[1] -(yearl))) # estimates of rli in years before the first assessment, based on random values of slopes 

      if (leftextrapol) leftslopes=c(leftslopes,useslp)  # to store the random value of slope generated in this iteration r - for future calculation to apply to corals
      datf=rbind(datf, data.frame(year=yearl, rli=irlel))  ## adds left-extrapolated rlis to table datf

      #with(datf, points(year, rli, col=2, pch=19, cex=.6))
      datf

      #right extrapolation - does exactly the same as previous, but to the years after the last assessment of group g until current year + 2
      yearr=c(years[which(years>max(yearsti))],max(years)+1,max(years)+2)   ## vector with years for right extrapolation: since year of latest assessment of group g until current year + 2
      yearr

      x1=sort(dat$year)[(nrow(dat)-1):nrow(dat)]   #last two years of assessment
      x1    ## years
      y1=c(dat$rli[dat$year==x1[1]],dat$rli[dat$year==x1[2]])   ### rli in last two years of assessment
      y1    ### rlis
      nv=lm(y1~x1)    # linear model of the rate of change in rli (based on last two years of assessment) to estimate slope and intercept for extrapolation
      mnslp <- coef(nv)[2]      # slope of the model
      useslp <- rnorm(1, mnslp, sd=slopecv*abs(mnslp))  # random value of slope based on the slope from the linear model and a potential standard devidation (set in the begining of the code - "slopecv") - changes each repetition
      irler=y1[2] - (useslp*(x1[2] -(yearr)))   # estimates of rli in years after the last assessment, based on random values of slopes 
      datf=rbind(datf, data.frame(year=yearr, rli=irler))   ## adds right-extrapolated rlis to table datf
      #with(datf, points(year, rli, col=2, pch=19, cex=.6))

      datf=datf[order(datf$year),]   # reorder the datf table by year


      if (plotit)  # plots the results if set to plot (i.e. if plotit=T, see lines above)
      {
        if (r<30) win.graph()  # only plots for the first 30 iterations
        with(datf, plot(year, rli, pch=19, col=2, main=paste(group,"s", sep="")))
        with(datf[datf$year%in%yearsti,], points(year, rli, pch=19, col=4))
      }

      restmw=NULL
      for (m in 3:(nrow(datf)-2)) restmw=c(restmw, sample(datf$rli[(m-2):(m+2)],1)) # moving average - considers a random rli from the interval of +- 2 years from each year - excludes the first and last two years in the final vector (but considers them for taking a random value)
      finalmw=restmw
      rliTot[rliTot$group==group&rliTot$nrep==r,]$rli=finalmw  # stores the rli per year y for group g in repetition r as the result of the moving average
      if (plotit) with(rliTot[rliTot$group==group&rliTot$nrep==r,], lines(year, rli, col=8))  # plots a line shwoing the values of the moving average if plotit=T

    }  #ends loop groups
  } #ends loop  nreps

  outpts[[1]]=rliTot       # stores the table with final rli value per year y, group g and repetition r in the list of outputs from this loop
  if (saveleftslope) outpts[[2]]=mean(leftslopes) # stores the mean values of left slopes (for coral) in the list of outputs from this loop

  return(outpts)
}

#### function to plot the rlis

plotrli=function(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=T, label=T){

difrgy=diff(range(basy, topy))
if (difrgy<miny) {
  basy=topy-miny
  difrgy=diff(range(basy, topy))}

with(ttuse,plot(year,rli, ylim=c(basy*.99,topy), xlim=c(min(year), max(year)+1), type="n", xlab="Year", yaxt="n", ylab="Red List Index of species survival"))
gs=unique(ttuse$group[ttuse$group!="aggregated"])
if(lu(gs)>0)
{
  for (g in 1:length(gs))
  {
    #g=1
    gs1=as.character(gs[g])
    res2=ttuse[ttuse$group==gs1,]
    with(res2,polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
  }
}
  
  if (aggreg) with(ttuse[ttuse$group=="aggregated",], polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz)) #don't plot if don't want aggregated line
  if(lu(gs)>0){
  for (g in 1:length(gs))
  {
    gs1=as.character(gs[g])
    res2=ttuse[ttuse$group==gs1,]
    with(res2,lines(year,rli, col=2, lwd=2))
    if (label) text(max(res2$year)+0.5, res2$rli[nrow(res2)],paste(gs1,"s", sep=""), cex=1, adj=c(0,0.5))
  }}
  if (aggreg) with(ttuse[ttuse$group=="aggregated",], lines(year,rli, col=4, lwd=2))
  
  if (lu(gs)==1&gs[1]=="Bird"){
  if (difrgy<0.3)
    {
      if (difrgy<0.1) {axis(2, seq(0,1, by=.002))} else axis(2, seq(0,1, by=.05))
    }
    if (difrgy>0.3) axis(2, seq(0,1, by=.1))
    par(mgp=c(3.5, 1, 0))
    title(xlab="Year")
       }
  else{
  if (difrgy<0.3) axis(2, seq(0,1, by=.05))
  if (difrgy>0.3) axis(2, seq(0,1, by=.1))
  }
}


# PART 4. RUN GLOBAL ANALYSES AND PREPARE PAST TABLES ("back-casting") ##############################
######################################################################################################

mink=1
if (only_thematic) mink=2
mink

for (k in mink:n_iter)  #### loop to go through all the disaggregations
{
 #k=2
  
  ### k=1 runs the non-thematic RLIs (all species and genuine changes)
  if (k ==1){       
    altpasttable=F    # if to build and use an alternative pastable, to use in 50% of the iterations; only valid for dissagregations based on drivers of genuine changes (to deal with secondary drivers)
    gench=genchF
    tbf1=sppp1
    print(k)
    name2save="general"
    finfolder=paste(finfolderP,"General", sep="/")
    print(finfolder)
    }
    
    ##iterations corresponding to the drivers of genuine changes
    if (length(driv_gc)>0){  
   if (k>1&k<(length(driv_gc)+2)) 
    {   
        altpasttable=T   # if to build and use an alternative pastable, to use in 50% of the iterations; only valid for dissagregations based on drivers of genuine changes (to deal with secondary drivers)
        driv=driv_gc[[(k-1)]]
        driv
        driv_name=names(driv_gc)[[k-1]]
        driv_name
        name2save=driv_name
        genchT=data.frame(genchF[,which(names(genchF)%in%driv)])
        head(genchT)
        length(genchT)
        gc2use_list=NULL
        gc2use_list_altern=NULL
        for (j in 1:length(genchT))  gc2use_list=c(gc2use_list,which(genchT[,j]%in%c("Primary","Most Important Secondary")))
        for (j in 1:length(genchT))  gc2use_list_altern=c(gc2use_list_altern,which(genchT[,j]%in%c("Primary","Most Important Secondary","Secondary")))
        length(gc2use_list)
        length(gc2use_list_altern)
        gc2use_list=sort(unique(gc2use_list))
        gc2use_list_altern=sort(unique(gc2use_list_altern))
        gench=genchF[gc2use_list,]
        gench_alt=genchF[gc2use_list_altern,]
        head(gench)
        nrow(gench)
        #head(gench_alt)
        nrow(gench_alt)
        tbf1=sppp1
        finfolder=paste(finfolderP,"Thematic",name2save, sep="/")
        finfolder
        print(k)
        print(finfolder)
  
    }}
     
     ##iterations corresponding to the groups of species
     if (length(splists)>0){ 
    if (k>(length(driv_gc)+1)&k<(length(driv_gc)+length(splists)+2)) 
    {   
    altpasttable=F     # if to build and use an alternative pastable, to use in 50% of the iterations; only valid for dissagregations based on drivers of genuine changes (to deal with secondary drivers)
    splist=splists[(k-1-length(driv_gc))]
    splist
    name2save=splist
    gench=genchF
    sp2use=tabsp1$Scientific[which(tabsp1[,which(names(tabsp1)==splist)]==1)]
    tbf1=sppp1[sppp1$Scientific%in%sp2use,]
    head(tbf1)
    lu(tbf1$Scientific)
    finfolder=paste(finfolderP,"Thematic",name2save, sep="/")
    print(k)
    print(finfolder)
        }}
      
    
    if (k>(length(driv_gc)+length(splists)+1)) ##iterations corresponding to combinations of drivers and groups of species
    {   
          altpasttable=T    # if to build and use an alternative pastable, to use in 50% of the iterations; only valid for dissagregations based on drivers of genuine changes (to deal with secondary drivers)
          comb=combs[(k-1-length(driv_gc)-length(splists)),]
          comb
          driv_name=comb[,1]
          driv_name
          driv=driv_gc[[which(names(driv_gc)==driv_name)]]
          driv
          genchT=data.frame(genchF[,which(names(genchF)%in%driv)])
          head(genchT)
          length(genchT)
          gc2use_list=NULL
          gc2use_list_altern=NULL
          for (j in 1:length(genchT))  gc2use_list=c(gc2use_list,which(genchT[,j]%in%c("Primary","Most Important Secondary")))
          for (j in 1:length(genchT))  gc2use_list_altern=c(gc2use_list_altern,which(genchT[,j]%in%c("Primary","Most Important Secondary","Secondary")))
          length(gc2use_list)
          gc2use_list=sort(unique(gc2use_list))
          length(gc2use_list)
          gench=genchF[gc2use_list,]
          length(gc2use_list_altern)
          gc2use_list=sort(unique(gc2use_list))
          gc2use_list_altern=sort(unique(gc2use_list_altern))
          gench_alt=genchF[gc2use_list_altern,]
          head(gench)
          nrow(gench)
          #head(gench_alt)
          nrow(gench_alt)
                    
          splist=comb[,2]
          splist
          sp2use=tabsp1$Scientific[which(tabsp1[,which(names(tabsp1)==splist)]==1)]
          tbf1=sppp1[sppp1$Scientific%in%sp2use,]
          head(tbf1)
          lu(tbf1$Scientific)
          
          name2save=paste(driv_name,splist, sep="_")
          finfolder=paste(finfolderP,"Thematic",name2save, sep="/")
          finfolder
          print(k)
    print(finfolder)
    }
    
groups=unique(tbf1$Group)   # taxonomic groups to consider in the analyses
groups

# 4.1. reconstruct past tables  - tables with all RL categories for each species in each year of assessment #################
pastables=data.frame()
for (g in  1:length(groups))      ### loop to run the code for each taxonomico group of speices
{
  #g=1
  group=as.character(groups[g])     ### to choose the group to analyse
  #group="Bird"
  group
  tbgn=gench[gench$Group==group,] ## table with relevant genuine changes for this group and disaggregation
  tbgn2=genchF[genchF$Group==group,] ## same as previous but all genuine changes for this group - to extract the years of assessment
  tball=tbf1[tbf1$Group==group,]  ### pp sp-cnts for this group
  nrow(tbgn)  # to check the number of genuine changes in this group
  head(tbgn)   # structure of table with genuine changes for this group
  yearsa=sort(unique(c(tbgn2$start_year,tbgn2$end_year)))
  yearsa   # years of RL assessments for this group 
  yearsa2=yearsa[1:length(yearsa)-1]
  yearsa2  # years of RL assessments for this group (excluding the last one)
  tabsp=with(tabsp1[tabsp1$Scientific%in%tball$Scientific,], data.frame(Scientific=Scientific, iucn=Cat_latest))  ## simplified table with the list of species of this group and latest iucn class 
 tabsp$year=max(yearsa)    ## field with the max year of assessment of the group (equal to all species)
  head(tabsp)               ## check structure of table tabsp
  
  sp1=unique(tball$Scientific)    ## list of the species in this group
  length(sp1)                       ## number of species in this group
  
  tabsp2=tabsp    # starting table with the latest iucn class - will be complemented with the status of the same species in the previous assessments in the next loop
  tabspnew=tabsp   # reference table with the list of species, iucn class and latest year of assessment 
  for (y in length(yearsa2):1)   # loop to go through all the years of assessments "y" except the last  (starting by the last)
  {
    #y=6
    tabsp3=tabspnew # reference table with the list of species, iucn class and latest year of assessment (starting with the most recent one in the first loop)
    tabsp3$year=yearsa2[y]    # changes year to year of the assessment y
    t2ch=tbgn[tbgn$start_year==yearsa2[y],c("Scientific","cat_start")]  # simplified table of genuine changes in year y
    head(t2ch)     # check structure of table
    nrow(t2ch)      # number of genuine changes in year y
    
    for (z in 1:nrow(t2ch))    # loop to go through all the genuine changes in year y
    {
      #z=1
      tabsp3$iucn[tabsp3$Scientific==as.character(t2ch$Scientific[z])]=as.character(t2ch$cat_start[z])  ## for species with genuine changes in year y, change table built above to the iucn class in that year y
    }
    tabspnew=tabsp3   # changes the reference table to account for the changes occured in year y - would feed the next iteration for previous year of assessment
    tabsp2=rbind(tabsp2,tabsp3)  ## adds iucn class of all sp in this group in year of assessment y to the starting table 
  }
  
  lu(yearsa)  ## number of years of comprehensive assessments
  nrow(tabsp2)/lu(tabsp2$Scientific) ## should be the number of years of comprehensive assessments (i.e., same as previous)
   tabsp2$group=group ## addign the name of the group to the table just created
  
   pastables=rbind(pastables,tabsp2)      ## merge all tables for different groups in a single table
}


if (altpasttable) {
pastables_alternative=data.frame()
for (g in  1:length(groups))      ### loop to run the code for each taxonomico group of speices
{
  #g=1
  group=as.character(groups[g])     ### to choose the group to analyse
  #group="Mammal"
  group
  tbgn=gench_alt[gench_alt$Group==group,] ## table with relevant genuine changes for this group and disaggregation
  tbgn2=genchF[genchF$Group==group,] ## same as previous but all genuine changes for this group - to extract the years of assessment
  tball=tbf1[tbf1$Group==group,]  ### pp sp-cnts for this group
  nrow(tbgn)  # to check the number of genuine changes in this group
  head(tbgn)   # structure of table with genuine changes for this group
  yearsa=sort(unique(c(tbgn2$start_year,tbgn2$end_year)))
  yearsa   # years of RL assessments for this group 
  yearsa2=yearsa[1:length(yearsa)-1]
  yearsa2  # years of RL assessments for this group (excluding the last one)
  tabsp=with(tabsp1[tabsp1$Scientific%in%tball$Scientific,], data.frame(Scientific=Scientific, iucn=Cat_latest))  ## simplified table with the list of species of this group and latest iucn class 
 tabsp$year=max(yearsa)    ## field with the max year of assessment of the group (equal to all species)
  head(tabsp)               ## check structure of table tabsp
  
  sp1=unique(tball$Scientific)    ## list of the species in this group
  length(sp1)                       ## number of species in this group
  
  tabsp2=tabsp    # starting table with the latest iucn class - will be complemented with the status of the same species in the previous assessments in the next loop
  tabspnew=tabsp   # reference table with the list of species, iucn class and latest year of assessment 
  for (y in length(yearsa2):1)   # loop to go through all the years of assessments "y" except the last  (starting by the last)
  {
    #y=6
    tabsp3=tabspnew # reference table with the list of species, iucn class and latest year of assessment (starting with the most recent one in the first loop)
    tabsp3$year=yearsa2[y]    # changes year to year of the assessment y
    t2ch=tbgn[tbgn$start_year==yearsa2[y],c("Scientific","cat_start")]  # simplified table of genuine changes in year y
    head(t2ch)     # check structure of table
    nrow(t2ch)      # number of genuine changes in year y
    
    for (z in 1:nrow(t2ch))    # loop to go through all the genuine changes in year y
    {
      #z=1
      tabsp3$iucn[tabsp3$Scientific==as.character(t2ch$Scientific[z])]=as.character(t2ch$cat_start[z])  ## for species with genuine changes in year y, change table built above to the iucn class in that year y
    }
    tabspnew=tabsp3   # changes the reference table to account for the changes occured in year y - would feed the next iteration for previous year of assessment
    tabsp2=rbind(tabsp2,tabsp3)  ## adds iucn class of all sp in this group in year of assessment y to the starting table 
  }
  
  lu(yearsa)  ## number of years of comprehensive assessments
  nrow(tabsp2)/lu(tabsp2$Scientific) ## should be the number of years of comprehensive assessments (i.e., same as previous)
   tabsp2$group=group ## addign the name of the group to the table just created
  
   pastables_alternative=rbind(pastables_alternative,tabsp2)      ## merge all tables for different groups in a single table
}
}  # ends alternative past tables
#write.csv(pastables_alternative, "pastables_alternative_2020.csv", row.names=F)  ## to save past tables if needed

#write.csv(pastables, "pastables_2019.csv", row.names=F)  ## to save past tables if needed
#pastables=read.csv("pastables.csv")         ## to read existing past tables
wex=wts$wg[wts$iucn=="EX"]          ## "weight" of extint species
head(pastables)                 # check the structure of past tables
tail(pastables)                 # check the structure of past tables

# 4.2  global RLI 
# this step also estimates left slope for CORALS (left slope for corals is calculated separately - cannot use steep recent declines for past extrapolation)


if ("Coral"%in%groups) groupsf=c(as.character(groups[groups!="Coral"]),"Coral")  # to ensure that Corals will be the last group to run (to use the mean slope for left extrapolation)
tt=proc.time()  ## to calculate the time it takes to run

### non-corals

groups2=as.character(groups[groups!="Coral"])   ## to exclude corals from the next calculations (left slope is calculated in a different way)
rliTotGlobal_noncorals_list=rlicalc(nreps=repetitions, groups2=groups2, years=years, leftextrapol=T, saveleftslope=T, plotit=F)   # calculates global rlis for all groups except corals 

rliTotGlobal_noncorals=rliTotGlobal_noncorals_list[[1]]  # table resulting from global rli calculations for all groups except corals (see code above)
head(rliTotGlobal_noncorals)
tail(rliTotGlobal_noncorals)

meanleftslopes=rliTotGlobal_noncorals_list[[2]]   # left slope estimate resulting from global rli calculations - to apply to corals (see code above)
meanleftslopes

### corals
if ("Coral"%in%groups){
rliTotGlobal_corals_list=rlicalc(nreps=repetitions, groups2="Coral", years=years, leftextrapol=F, saveleftslope=F, plotit=F)   # calculates global rlis for corals (using left slope for extrapolation based on average of other groups)
rliTotGlobal_corals=rliTotGlobal_corals_list[[1]]  # table resulting from global rli calculations for corals (see code above)
head(rliTotGlobal_corals)
tail(rliTotGlobal_corals)
}

##### combines results for corals and other groups 
rliTotGlobal=rliTotGlobal_noncorals
if ("Coral"%in%groups) rliTotGlobal=rbind(rliTotGlobal_noncorals,rliTotGlobal_corals) 


### aggregated rli
rlitotagrs=with(rliTotGlobal, aggregate(rli,list(year=year, nrep=nrep), mean))  # estimates aggregated rlis per year and per repetition as the average across groups 
names(rlitotagrs)[length(rlitotagrs)]="rli"
rlitotagrs$group="aggregated"
head(rlitotagrs)


## table with rli for each group and aggregated 
rliTotF=rbind(rliTotGlobal,rlitotagrs[,c("group","year","nrep","rli")])  ## combines rli for each group and aggregated in a single table
head(rliTotF)
tail(rliTotF)
max(rliTotF$nrep)# number of repetitions
max(rliTotF$rli)  # maximum rli value 


### final table with the median rli and 95% intervals (based on the repetitions) per year and group 
resg=with(rliTotF, aggregate(rli, list(year=year, group=group), length))   # n repetitions
resg$rli=with(rliTotF, aggregate(rli, list(year=year, group=group), median))$x    # median rli
resg$qn95=with(rliTotF, aggregate(rli, list(year=year, group=group), q95))$x   # quantile 95%
resg$qn05=with(rliTotF, aggregate(rli, list(year=year, group=group), q05))$x  # quantile 05%
resg

gs=unique(resg$group)

# to restructure the final table with the rli to use only the years between the first and last assessment of each group (for aggregated rli, starts 10 years before the first year of assessment of the latest group starting being assessed) 
resgf=data.frame()
for (g in 1:lu(gs))
{
  #g=1
  gs1=as.character(gs[g])
  gs1
  if (gs1!="aggregated") yrs=min(genchF$start_year[genchF$Group==gs1]):max(genchF$end_year[genchF$Group==gs1]) else yrs=(max(with(genchF, aggregate(start_year, list(Group), min))$x)-10):maxyear
  rli3=resg[resg$group==gs1&resg$year%in%yrs,]
  rli3=rli3[order(rli3$year),]
  resgf=rbind(resgf,rli3)
}
head(resgf,20)


## to plot the global rlis
tab2plot=resgf
par(las=1)
with(tab2plot,plot(year,rli, ylim=c(min(qn05), max(qn95)), xlim=c(min(year), max(year)+4), type="n", xlab="Year", ylab="Red List Index of species survival"))

## plots first the error area
gs=unique(tab2plot$group[tab2plot$group!="aggregated"])
lu(gs)
for (g in 1:length(gs))
{
  #g=1
  gs1=as.character(gs[g])
  res2=tab2plot[tab2plot$group==gs1,]
  with(res2,polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
}
with(tab2plot[tab2plot$group=="aggregated",], polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))

### add the lines to the plot
for (g in 1:length(gs))
{
  gs1=as.character(gs[g])
  res2=tab2plot[tab2plot$group==gs1,]
  with(res2,lines(year,rli, col=2, lwd=2))
  text(max(res2$year)+0.5, res2$rli[nrow(res2)],paste(gs1,"s", sep=""), cex=.6, adj=c(0,0.5))
}
with(tab2plot[tab2plot$group=="aggregated",], lines(year,rli, col=4, lwd=2))
#abline(v=2015, lty=3, col=8)
#write.csv(resgf, "global_RLIs.csv", row.names=F)
#pastables=read.csv("pastables.csv")
#meanleftslopes=(-0.0009302144)

#####

# 4.3 exports csv, pdf and png with global rlis

rlinew=resgf
final2save=rlinew[, c("group","year","rli","qn05","qn95")]

## 4.3.1 to save the global csvs



finfolderF=paste(finfolder, "/", "global/csv_tables", sep="")
finfolderF
if (saveglobal){
if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)
finalname=paste(finfolderF, "/", "global_all_taxa_", name2save,".csv", sep="")
finalname
write.csv(final2save,finalname, row.names=F)

final2save2=rlinew[rlinew$group=="aggregated", c("group","year","rli","qn05","qn95")]
finalname=paste(finfolderF, "/", "global_aggregated_", name2save,".csv", sep="") #edit to _wo_agg to plot without aggregated line
finalname
write.csv(final2save2,finalname, row.names=F)
}

## 4.3.2 to save the global pdfs

## all taxa and aggregated
if (saveglobal) {
if (pdfs) { 

finfolderF=paste(finfolder, "/", "global/pdfs", sep="")
finfolderF
if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)

#graph all taxa and aggregated
pdf1name=paste(finfolderF, "/","global","_all_taxa_",name2save,".pdf", sep="") 
pdf1name
pdf(file=pdf1name, width = 12, height = 10)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=T) 
dev.off()


#graph all taxa withouth aggregated
pdf1name=paste(finfolderF, "/","global","_all_taxa_without_aggr_",name2save, ".pdf", sep="") 
pdf1name
pdf(file=pdf1name, width = 12, height = 10)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=F)
 dev.off()


#graph aggregated
pdf2name=paste(finfolderF,"/", "global","_aggregated_",name2save, ".pdf", sep="")
pdf2name
pdf(file=pdf2name, width = 12, height = 10)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="aggregated",], aggreg=T)
dev.off()


# graph birds
pdf3name=paste(finfolderF,"/", "global","_birds_",name2save, ".pdf", sep="")
pdf3name
pdf(file=pdf3name, width = 12, height = 10)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,7,4,2), mgp=c(5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="Bird",], aggreg=T, label=F)
dev.off()

} ### ends condition pdfs=T
} # ends condition saveglobal=T


## 4.3.3  to save the global pngs

if (saveglobal) {
if (pngs) { 

finfolderF=paste(finfolder, "/", "global/pngs", sep="")
finfolderF
if (dir.exists(finfolderF)==F) dir.create(finfolderF)

### png all taxa with aggregated
png1name=paste(finfolderF, "/","global","_all_taxa_",name2save, ".png", sep="")
png1name
png(file=png1name, width = 12, height = 10, units="in", res=600)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=T) 
dev.off()

#graph all PNG without aggregated line (commented out)
png1name=paste(finfolderF, "/","global","_all_taxa_without_aggr_",name2save, ".png", sep="")
png1name
png(file=png1name, width = 12, height = 10, units="in", res=600)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=F, label=T)              ### all without aggregated
dev.off()


#graph aggregated png
png2name=paste(finfolderF,"/", "global","_aggregated_",name2save, ".png", sep="") #edit to _wo_agg to plot without aggregated line
png2name
png(file=png2name, width = 12, height = 10, units="in", res=600)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="aggregated",], aggreg=T)    # only aggregated
dev.off()


# graph birds PNG
png3name=paste(finfolderF,"/", "global","_birds_",name2save, ".png", sep="")
png3name
png(file=png3name, width = 12, height = 10, units="in", res=600)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,7,4,2), mgp=c(5, 1, 0))
plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="Bird",], aggreg=T, label=F)   # only birds
dev.off()

}  ### ends condition pngs=T
} # ends condition saveglobal=T


 
# PART 5. RUN ANALYSES PER REGION/COUNTRY #############################################
############################################################################################# 

# 5.1 run analyses per region/country (i.e., combinations of isos)

if (k==1 | dis_subglobal){
if  (length(isos2use)>0) {   ### will only run if there's any isos2use set
 for (f in 1:length(isos2use))    ##  ### starts the loop to go through all the combinations if isos (countries or regions, etc - defined in isos2use vector at the start)
 {

#f=1
iso2use=isos2use[f]
iso2use
isos$isof=isos[,which(names(isos)==iso2use)]  
head(isos)    ###  check structure of isos table

# check if ISOs in sp-cnt table and isos tables are the same
lu(isos$isof) ###  check number of unique iso codes (or groups, regions) in isos table

## verify if the number of iso codes in pps sp-countries is the same as in isos 
lu(tbf1$ISO3) ##number of unique iso codes in table with the pps sp-countries (should be the same or one less - ABNJ - comparing with the isos)
isos$ISO3[which(!isos$isof%in%tbf1$ISO3)] # confirm that the difference in isos between tables (isos and sp-countries-pp) is due to the ABNJ; only valid for global RLIs
sort(unique(tbf1$ISO3[which(!tbf1$ISO3%in%isos$ISO3)])) # confirm that the difference in isos between tables (isos and sp-countries-pp) is due to the ABNJ; only valid for global RLIs


#merge tbf1 with isos 
nrow(tbf1)   ###   check number of rows in table with the pps sp-countries (tbf1)
head(tbf1)   ###    check check structure of table with the pps sp-countries
tbf2=merge(tbf1, isos[,c("ISO3","isof")],by="ISO3",all.x=T)
nrow(tbf2)   ###   check number of rows in table with the pps sp-countries (tbf1)  after the merge
head(tbf2)   ###    check check structure of table with the pps sp-countries    after the merge


nreps=repetitions
plotit=F
plotit2=T
finalrlis=data.frame()

cts=unique(tbf2$isof)    ### group of countries, regions or subregions (set in isof)  - hereafter "cts"
lu(cts)
cts=as.character(cts)
cts[is.na(cts)]="0"
cts=cts[cts!="0"]
cts
torun=cts
others=NULL

##### to check differences between ISO_BL and ISO_SDG
if ("ISO_BL"%in%isos2use & "ISO_SDG"%in%isos2use & iso2use=="ISO_SDG"){

difs=NULL
for (p in 1:nrow(isos))
{
#p=1
#p=3
dif1=grep(pattern=as.character(isos$ISO_BL[p]), as.character(isos$ISO_SDG[p])) # check the match between the iso_BL code and iso_SDG code for each iso3; 1 if there's a match; NULL if not
dif1
if(length(dif1)==0) dif1=2  ## attributes a value of 2 if there's no match
difs=c(difs, dif1)
}
difs=difs-1    # change vector of differences to: 1=different iso codes, 2=same iso codes
length(difs)
nrow(isos)
sum(difs) # number of isos that differ
head(isos,20)

difisos=sort(unique(isos$ISO_SDG[difs==1]))
torun=unique(tbf2$isof)[unique(tbf2$isof)%in%difisos]
others=unique(tbf2$isof)[!unique(tbf2$isof)%in%difisos]
}
torun
others

tt=proc.time()

#SDG Diff to BL: ('FIN','GBR','FRA','NOR','AUS','CHN','USA')

if (length(cts)>0){   # if there's any cnt to run in this iso2use (for disaggregated RLIs can happen that there's no country to run, e.g. if all endemics of non-LLDC and non-SIDS countries)
for (x in 1:length(cts)) #length(cts)     ### loop to go through all cts 
  #for (x in 1:4)
{
  #x=1
  #x=which(cts=="ARE")
  cnt=cts[x]       # to set the cts to be analysed in this iteraction
  cnt
  print(paste(x, cnt))
  
  if (cnt %in%torun)  {
  tbct=tbf2[tbf2$isof==cnt,]     ## table with the list of species  of this cnt and respective pp of the distribution
  
   # to calculate the number of species and genuine changes in this country/region - to save in the final csv file
   nspecies=with(tbct, aggregate(Scientific, list(group=group),lu))
      names(nspecies)[length(nspecies)]="n_sp"
   nspecies
    genchtable=gench[gench$Scientific%in%tbct$Scientific,]
    if (nrow(genchtable)>0){
   ngenchanges=with(genchtable, aggregate(Scientific, list(group=Group),lu))
   names(ngenchanges)[length(ngenchanges)]="n_gen_changes"
   ngenchanges
   nspecies=merge(nspecies, ngenchanges, all.x=T, all.y=T)
   nspecies$n_gen_changes[is.na(nspecies$n_gen_changes)]=0
   }
   if (nrow(genchtable)==0)   nspecies$n_gen_changes=0
   nspecies
   nspecies=rbind(nspecies, data.frame(group="aggregated", n_sp=sum(nspecies$n_sp), n_gen_changes=sum(nspecies$n_gen_changes)))
   nspecies
   nspecies$ordem=1:nrow(nspecies)
    
  #unique groups
  groupsf=as.character(unique(tbct$Group))  # unique groups existing in this cnt  (not all taxonomic groups exist in all cnts)
  groupsf<-na.omit(groupsf)
  groupsf
  
  ## code to calculate the rli for this cnt  - see similar code above for notes  ("function to calculate global RLI"); main differences commented (related with the adaptation for taking into consideration the pp of each species in each cnt)
  rliext=data.frame(country=cnt, group=rep(rep(groupsf, each=length(years)),nreps), year=rep(rep(years, length(groupsf)),nreps), nrep=rep(1:nreps,each=length(years)*length(groupsf)), rli=NA)
  head(rliext)
  tail(rliext)
  
  for (r in 1: nreps) #nreps
  {
    #r=1
    print(paste(x, cnt, r))
     
    for (g in 1:length(groupsf))      ### note that loop of groups has to be the first, to save the slopes to apply to corals
    {
      #g=1
      group=as.character(groupsf[g])
      group
      spcnt=tbct[tbct$Group==group,]
      lu(spcnt$Scientific)
      nrow(spcnt)
      spcnt2=with(spcnt, aggregate(pp, list(Scientific=Scientific), sum))  ## sum pp for species and country (could be split for marine/terrestrial or disjucnt polygons in country/eez layer with multiple entries)
      nrow(spcnt2)
      names(spcnt2)[length(spcnt2)]="pp"
      head(spcnt2)
      #max(spcnt2$pp)
      spcnt2[spcnt2$pp==1,]   ## endemic species
      spcnt3=merge(spcnt2,pastables)
      if (altpasttable & ((r/2)-floor(r/2))==0) spcnt3=merge(spcnt2,pastables_alternative)  # to alternative past tables in half of the iterations, for disaggregated rli related to drivers of genuine changes
      head(spcnt3)
      tail(spcnt3)
      nrow(spcnt3)/nrow(spcnt2) ## should be the number of years of assessment for this group
      yearsa=sort(unique(spcnt3$year))
      yearsa
      yearsa2=yearsa[1:length(yearsa)-1]
      yearsa2
      
      ### years of assessment
      
      rlig=data.frame()
      for (y in 1:length(yearsa))
      {
        #y=1
        year=yearsa[y]
        year
        
        tb=spcnt3[spcnt3$year==year,]
        head(tb)
        nrow(tb)
        
        unique(tb$iucn)
        
        tb[tb$iucn=="DD",]
        tb[!tb$iucn%in%c("DD", "LC"),]
        tb[tb$iucn=="LC",]
        
        tb=merge(tb,wts)
        head(tb)
        nrow(tb)
        dds=tb$Scientific[tb$iucn=="DD"]
        dds
        head(tb[tb$iucn=="DD",])
        allwgs=tb$wg[tb$wg>(-1)]
        if (length(allwgs)==0) allwgs=0   ## if all species of this group in this cnt are DD
        
        if (length(dds)>0) { 
        tbndd=tb[tb$iucn!="DD",]
        tbdd=tb[tb$iucn=="DD",]
        tbdd$wg=sample(allwgs,nrow(tbdd), replace=T)
        head(tbdd)
        tb=rbind(tbndd, tbdd)
        
        #for (d in 1:length(dds)) tb$wg[tb$Scientific==dds[d]]=sample(allwgs,1) # to replace the DD species by another category, randomly selected by the pool of other species
        
        }
        #tb[tb$iucn=="DD",]
        
        tb$pp<-unlist(tb$pp)
        tb$wxpp=tb$wg*tb$pp  ## weight of the change multiplied by the pp of the species in cnt
        head(tb)
                sum(tb$wxpp)
        wex*sum(tb$pp)
        sum(tb$wxpp)/(wex*sum(tb$pp))
        1-round(sum(tb$wxpp)/(wex*sum(tb$pp)),7)
        rli1=1-round((sum(tb$wxpp)/(wex*sum(tb$pp))),7)
        rli1
        crli=round((sum(tb$wxpp)/(wex*sprich$rich[sprich$group==group])),7)  
        rlig=rbind(rlig, data.frame(group=group,year=year, rli=rli1))
        
        if (y==1) tb1=tb
        
      }  ## ends loop year
      rlig
      dat=rlig
      dat
      
      #interpolation
      yearsti=min(dat$year): max(dat$year)
      irli=approx(dat$year, dat$rli, xout=yearsti, method="linear", rule=2:2)$y
      irli
      
      datf=data.frame(year=yearsti, rli=irli)
      datf
      #with(datf, plot(year, rli, xlim=c(1975,2018), ylim=c(0.6,1.2)))
      
      #extrapolation    # y=a+bx
      #left
      yearl=c(min(years)-2, min(years)-1, years[which(years<min(yearsti))])
      yearl
      
      x1=sort(dat$year)[1:2]
      x1   ### years
      y1=c(dat$rli[dat$year==x1[1]],dat$rli[dat$year==x1[2]])
      y1    ###rlis
      nv=lm(y1~x1)
      mnslp <- coef(nv)[2]
      useslp <- rnorm(1, mnslp, sd=slopecv*abs(mnslp))
      if (group=="Coral") useslp <- meanleftslopes
      irlel=y1[1] - (useslp*(x1[1] -(yearl)))
      
      datf=rbind(datf, data.frame(year=yearl, rli=irlel))
      datf
      
      #right
      yearr=c(years[which(years>max(yearsti))],max(years)+1,max(years)+2)
      yearr
      
      x1=sort(dat$year)[(nrow(dat)-1):nrow(dat)]
      x1    ## years
      y1=c(dat$rli[dat$year==x1[1]],dat$rli[dat$year==x1[2]])
      y1    ### rlis
      nv=lm(y1~x1)
      mnslp <- coef(nv)[2]
      useslp <- rnorm(1, mnslp, sd=slopecv*abs(mnslp))
      irler=y1[2] - (useslp*(x1[2] -(yearr)))
      datf=rbind(datf, data.frame(year=yearr, rli=irler))
      datf=datf[order(datf$year),]
      
      if (plotit)
      {
        win.graph()
        with(datf, plot(year, rli, pch=19, col=2, main=paste(group,"s", sep="")))
        with(datf[datf$year%in%yearsti,], points(year, rli, pch=19, col=4))
      }
      
      restmw=NULL
      for (m in 3:(nrow(datf)-2)) restmw=c(restmw, sample(datf$rli[(m-2):(m+2)],1))
      finalmw=restmw
      rliext[rliext$group==group&rliext$nrep==r,]$rli=finalmw
      #rliext[rliext$group==group&rliext$nrep==r&!is.na(rliext$group),]$rli=finalmw
      if (plotit) with(rliext[rliext$group==group&rliext$nrep==r,], lines(year, rli, col=8))
    } # ends loop group
  } # ends loop rep
  
  head(rliext)
  tail(rliext)
  max(rliext$nrep)
  max(rliext$rli)
  
  rlitotagrs=with(rliext, aggregate(rli,list(year=year, nrep=nrep), mean))
  names(rlitotagrs)[length(rlitotagrs)]="rli"
  rlitotagrs$group="aggregated"
  rlitotagrs$country=cnt
  head(rlitotagrs)
  
  rliextF=rbind(rliext,rlitotagrs[,c("country","group","year","nrep","rli")])
  head(rliextF)
  tail(rliextF)
  max(rliextF$nrep)
  max(rliextF$rli)
  
  resg=with(rliextF, aggregate(rli, list(year=year, group=group), length))
  resg$rli=with(rliextF, aggregate(rli, list(year=year, group=group), median))$x
  resg$qn95=with(rliextF, aggregate(rli, list(year=year, group=group), q95))$x
  resg$qn05=with(rliextF, aggregate(rli, list(year=year, group=group), q05))$x
  head(resg)
  
  ## to correct estimated rlis that went out of the bounds [0-1] due to extrapolation
  resg$rli[resg$rli>1]=1
  resg$rli[resg$rli<0]=0
  resg$qn95[resg$qn95>1]=1
  resg$qn95[resg$qn95<0]=0
  resg$qn05[resg$qn05>1]=1
  resg$qn05[resg$qn05<0]=0
  
  gs=unique(resg$group)
  resgf=data.frame()
  for (g in 1:lu(gs))
  {
    #g=1
    gs1=as.character(gs[g])
    gs1
    if (gs1!="aggregated") yrs=min(genchF$start_year[genchF$Group==gs1]):max(genchF$end_year[genchF$Group==gs1]) else yrs=(max(with(genchF, aggregate(start_year, list(Group), min))$x)-10):maxyear
    rli3=resg[resg$group==gs1&resg$year%in%yrs,]
    rli3=rli3[order(rli3$year),]
    resgf=rbind(resgf,rli3)
  }
  head(resgf,30)
  
  final=resg
  final$country=cnt
  head(final)
  final=merge(final, nspecies)
  final=final[order(final$ordem, final$year),]
  finalrlis=rbind(finalrlis,final)
  
  
  # 5.2 save the csv for each region/country
  
  final2save=final[, c("country","group","year","rli","qn05","qn95","n_sp","n_gen_changes")]
  
  if (k==1) finfolder=paste(finfolderP,"Subglobal", sep="/")
  finfolderF=paste(finfolder, "/", iso2use, "/csv_tables", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)
  
  #finalname=paste(finfolderF, "/", iso2use, "_", cnt,".csv", sep="")
  finalname=paste(finfolderF, "/", iso2use, "_", cnt,"_", name2save,".csv", sep="")
  finalname                                                               
  write.csv(final2save,finalname, row.names=F)
  
  if (plotit2){
    if (div0(x,6)==0)
    {
      win.graph(50,30)
      par(mfrow=c(2,3), mar=c(2,2,2,1))
    }
    
    tab2plot=resgf
    with(tab2plot,plot(year,rli, ylim=c(min(qn05), max(qn95)), xlim=c(min(year), max(year)+4), type="n", xlab="Year", ylab="Red List Index of species survival", main=cnt))
    
    gs=unique(tab2plot$group[tab2plot$group!="aggregated"])
    lu(gs)
    for (g in 1:length(gs))
    {
      #g=1
      gs1=as.character(gs[g])
      res2=tab2plot[tab2plot$group==gs1,]
      with(res2,polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
    }
    with(tab2plot[tab2plot$group=="aggregated",], polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
    
    for (g in 1:length(gs))
    {
      gs1=as.character(gs[g])
      res2=tab2plot[tab2plot$group==gs1,]
      with(res2,lines(year,rli, col=2, lwd=2))
      text(max(res2$year)+0.5, res2$rli[nrow(res2)],paste(gs1,"s", sep=""), cex=.6, adj=c(0,0.5))
    }
    with(tab2plot[tab2plot$group=="aggregated",], lines(year,rli, col=4, lwd=2))
  }
  } #ends loop if  (cnt %in%torun)

  if (cnt %in%others)  {   # to simply read the csv already saved if iso is the same as used in previous loops (e.g. for same isos in ISO_BL and ISO_SDG)

  finfolderF=paste(finfolder, "/", "ISO_BL", "/csv_tables", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)
    finalname=paste(finfolderF, "/", "ISO_BL", "_", cnt, "_", name2save,".csv", sep="")
  finalname
  final2save=read.csv(finalname)

  finfolderF=paste(finfolder, "/", iso2use, "/csv_tables", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)
  
  finalname=paste(finfolderF, "/", iso2use, "_", cnt, "_", name2save,".csv", sep="")
  finalname
  write.csv(final2save,finalname, row.names=F)
 
  } #ends loop to read files already saved in ISO_BL folder

} # ends loop country

print((proc.time()-tt)[1]/60) ## time in minutes

# 5.3 creates single table with all regions/countries
# reads all csvs per cnt and merges into a single table "finalrlis"

fls=dir(finfolderF)
finalrlis=data.frame()
for (y in 1:lu(fls)) {
  finalrlis=rbind(finalrlis, read.csv(paste(finfolderF, fls[y], sep= '/')))
}

str(finalrlis)
head(finalrlis)
lu(finalrlis$country)        

max(finalrlis$rli)
min(finalrlis$rli)

#######################################################################################################

#combine all files in 1 (in case the country files are calculated in different machines, to save time ) - DON'T FORGET TO SET THE iso2use !!

cmbfiles=F
#cmbfiles=T   

if(cmbfiles){
  iso2use="ISO_BL"    ### set which iso2use to combine
  finfolderF=paste(finfolder, "/", iso2use, "/csv_tables", sep="")
  finfolderF
  isos$isof=isos[,which(names(isos)==iso2use)]  
  tbf2=merge(tbf1, isos[,c("ISO3","isof")],by="ISO3",all.x=T)
  allfics=data.frame()
  fics=dir(finfolderF, pattern = ".csv", full.names =T)   ### copy paste here the folder with the countries' files
  for (u in 1:length(fics))  allfics=rbind(allfics, read.csv(fics[u]))
  lu(allfics$country)
  finalrlis=allfics
}

str(finalrlis)
head(finalrlis)
lu(finalrlis$country)
max(finalrlis$rli)


###################################################
# 5.4 save final graphs per region/country in pdf and png

iso2use
sumspp=with(tbf2, aggregate(pp, list(country=isof, group=Group), sum))
names(sumspp)[length(sumspp)]="sumpp"
head(sumspp)
sum(sumspp$sumpp)

rliextf=finalrlis
min(rliextf$rli)
max(rliextf$rli)
head(rliextf)
tail(rliextf)

plotit=T
writefiles=T
if (pdfs) plotit=F

allcountries_RLI=data.frame()
allcountries_CRLI=data.frame()

cts=unique(rliextf$country)
length(cts)

graphics.off()
for (i in 1:length(cts))    #length(cts)  ### loop to go through all cnts to create pngs, pdfs and calculate the national contribution to the global RLI (crli)
{
  #i=1
  #i=which(cts=="ABW")
  i
  
  ct1=as.character(cts[i])
  ct1
  rli2=rliextf[rliextf$country==ct1,]
  
  namect1=as.character(ct1)     ## name to appear in the graphic
  if (iso2use=="ISO_BL") namect1=paste(isos$uname_BL[isos$isof==as.character(ct1)][1] ," (", ct1, ")", sep="")
  if (iso2use=="ISO_SDG") namect1=paste(isos$uname_SDG[isos$isof==as.character(ct1)][1] ," (", ct1, ")", sep="")
  namect1
      
  gs=unique(rli2$group)
  lu(gs)
  
  rlinew=data.frame()
  crlis=data.frame()
  ### loop to estimate the crli for each group g in cnt i
  for (g in 1:lu(gs))
  {
    #g=1
    gs1=as.character(gs[g])
    gs1
    if (gs1!="aggregated") yrs=min(genchF$start_year[genchF$Group==gs1]):max(genchF$end_year[genchF$Group==gs1]) else yrs=(max(with(genchF, aggregate(start_year, list(Group), min))$x)-10):maxyear
    rli3=rli2[rli2$group==gs1&rli2$year%in%yrs,]
    rli3=rli3[order(rli3$year),]
    
    if (gs1!="aggregated")
    {
      crli1=rli2[rli2$group==gs1,]
      crli1$crli=crli1$rli*sumspp$sumpp[sumspp$group==gs1&sumspp$country==ct1][1]/sprich$rich[sprich$group==gs1]
      crli1$crli05=crli1$qn05*sumspp$sumpp[sumspp$group==gs1&sumspp$country==ct1][1]/sprich$rich[sprich$group==gs1]
      crli1$crli95=crli1$qn95*sumspp$sumpp[sumspp$group==gs1&sumspp$country==ct1][1]/sprich$rich[sprich$group==gs1]
      crlis=rbind(crlis,crli1[,c("country","group","year","crli","crli05","crli95")])
    }
    rlinew=rbind(rlinew,rli3)
  }
  rlinew
  tail(crlis)
  
  # aggregated crli
  crlisag=with(crlis, aggregate(crli, list(country=country,  year=year), mean)) #
  names(crlisag)[length(crlisag)]="crli"
  crlisag$group="aggregated"
  crlisag$crli05=with(crlis, aggregate(crli05, list(country=country,  year=year), mean))$x
  crlisag$crli95=with(crlis, aggregate(crli95, list(country=country,  year=year), mean))$x
  
  yrs=(max(with(gench, aggregate(start_year, list(Group), min))$x)-10):maxyear
  crlisag=crlisag[crlisag$year%in%yrs,]
  crlisag
  crlisag[,c("country","group","year","crli","crli05","crli95")]
  
  crlisf=data.frame()
  gs2=unique(crlis$group)
  for (g in 1:length(gs2))
  {
    gs1=as.character(gs2[g])
    yrs=min(genchF$start_year[genchF$Group==gs1]):max(genchF$end_year[genchF$Group==gs1])
    crlisf=rbind(crlisf, crlis[crlis$group==gs1&crlis$year%in%yrs,])
  }
  crlisf=rbind(crlisf,crlisag)  ## final table with crli for each group and aggregated
  
  finfolderF=paste(finfolder, "/", iso2use, "/csv_tables_crlis", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)

  rlinew2save=rlinew[, c("country","group","year","rli","qn05","qn95")]
  
  #fnamect=paste(finfolderF, "/", iso2use, "_", ct1,".csv", sep="")
  #fnamect
  #if (writefiles) write.csv(rlinew2save,fnamect, row.names=F)
  
  fnamect2=paste(finfolderF,"/", iso2use, "_", ct1,"_crli_",name2save,".csv", sep="")
  fnamect2
  if (writefiles) write.csv(crlisf,fnamect2, row.names=F)
  
  allcountries_RLI=rbind(allcountries_RLI,rlinew2save)
  allcountries_CRLI=rbind(allcountries_CRLI,crlisf)
  
  if (plotit) {
    if (div0(i,4)==0)
    {
      win.graph(30,20)
      par(las=1, mfrow=c(4,4))
    }}
  
     
  ##5.4.1 to save the pdfs for this cnt 
  if (pdfs){
  
  finfolderF=paste(finfolder, "/", iso2use, "/pdfs", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)
   
       finfolderF2=paste(finfolderF, "/", iso2use, "_", ct1, sep="")
       finfolderF2
  
  #graph all taxa
  pdf1name=paste(finfolderF2,"_all_taxa_",name2save,".pdf", sep="")
  pdf1name
  pdf(file=pdf1name, width = 12, height = 10)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
  plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=T, label=T)     ### all w aggregated  
  dev.off()
  
  #graph aggregated
  pdf2name=paste(finfolderF2,"_aggregated_",name2save,".pdf", sep="")
  pdf2name
  pdf(file=pdf2name, width = 12, height = 10)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
  plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="aggregated",], aggreg=T)    # only aggregated
  dev.off()


   # graph birds
  pdf3name=paste(finfolderF2,"_birds_",name2save,".pdf", sep="")
  pdf3name
  #miny=.05
  pdf(file=pdf3name, width = 12, height = 10)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
  plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="Bird",], aggreg=T, label=F)   # only birds
  dev.off()
  

   ### crli
  
  pdf3name=paste(finfolderF2,"_crli_",name2save,".pdf", sep="")
  pdf3name
  pdf(file=pdf3name, width = 12, height = 10)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,6,4,2), mgp=c(4.5, 1, 0))
  
  with(crlisf,plot(year,crli, ylim=c(min(crli05),max(crli05)), xlim=c(min(year), max(year)+1), type="n", xlab="", ylab="National contribution to the global RLI"))
  par(mgp=c(3.5, 1, 0))
  title(xlab="Year")
  
  gs=unique(crlisf$group[crlisf$group!="aggregated"])
  lu(gs)
  for (g in 1:length(gs))
  {
    #g=1
    gs1=as.character(gs[g])
    res2=crlisf[crlisf$group==gs1,]
    with(res2,polygon(c(year,rev(year)),c(crli05,rev(crli95)), col=cinz, border=cinz))
  }
  with(crlisf[crlisf$group=="aggregated",], polygon(c(year,rev(year)),c(crli05,rev(crli95)), col=cinz, border=cinz))
  
  for (g in 1:length(gs))
  {
    gs1=as.character(gs[g])
    res2=crlisf[crlisf$group==gs1,]
    with(res2,lines(year,crli, col=2, lwd=2))
    if (plotit) text(max(res2$year)+0.5, res2$crli[nrow(res2)],paste(gs1,"s", sep=""), cex=.6, adj=c(0,0.5))
    if (pdfs) text(max(res2$year)+0.5, res2$crli[nrow(res2)],paste(gs1,"s", sep=""), cex=1, adj=c(0,0.5))
  }
  with(crlisf[crlisf$group=="aggregated",], lines(year,crli, col=4, lwd=2))
    dev.off()

  } # ends loop if pdfs



    #5.4.2 to save the pngs for this cnt 
  if (pngs){
  
   finfolderF=paste(finfolder, "/", iso2use, "/pngs", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)
   
       finfolderF2=paste(finfolderF, "/", iso2use, "_", ct1, sep="")
       finfolderF2
      
      ##### png all groups and aggregated
  png2name=paste(finfolderF2,"__all_taxa_",name2save,".png", sep="")
  png2name
  png(file=png2name, width = 12, height = 10, units="in", res=600)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
  plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew, aggreg=T, label=T)     ### all w aggregated
  dev.off()
  
    
  
  ####graph aggregated png
  png2name=paste(finfolderF2,"_aggregated_",name2save,".png", sep="")
  png2name
  #if (pngs) png(file=png2name, width = 12, height = 10)
  png(file=png2name, width = 12, height = 10, units="in", res=600)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
  plotrli(miny=.15, topy=min(c(1.02, max(rlinew$qn95)*1.1)), basy=min(rlinew$qn05)*.99, ttuse=rlinew[rlinew$group=="aggregated",], aggreg=T)    # only aggregated
  dev.off()
  
  
  #graph birds png changing y-axis
  png3name=paste(finfolderF2,"_birds_",name2save,".png", sep="")
  png3name
  png(file=png3name, width = 12, height = 10, units = "in", res = 600)
  par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))
  
  ttuse=rlinew[rlinew$group=="Bird",]
  if (nrow(ttuse)==0) plot(1:10, 1:10, type="n", main="no data for birds", xaxt="n", yaxt="n", xlab="", ylab="")
  if (nrow(ttuse)>0){
    
    topy=min(max(ttuse$qn95))
    basy=min(ttuse$qn05)
    
    topyR <- 0.05*ceiling(topy/0.05) # round up to nearest 0.05 for top of y-axis
    basyR <- 0.05*floor(basy/0.05) # round down to nearest 0.05 for base of y-axis
               
    if (topyR > 1.00){ # if top value of y axis is greater than 1, set to 1 (true value cannot be higher, only artificially made higher by adding arbitrary values onto the maximum RLI value)
      topyR <- 1.00
    }
    
      difrgy=diff(range(basy, topy))
     
    with(ttuse,plot(year,rli, yaxt="n", ylim=c(basyR,topyR), type="n", xlab="Year", ylab="Red List Index of species survival"))
    with(ttuse, polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
    with(ttuse, lines(year,rli, col=2, lwd=2))
    if (difrgy<0.3) axis(2, seq(0,1, by=.05))
    if (difrgy>0.3) axis(2, seq(0,1, by=.1))
  }
   dev.off()  
    
  } # ends  loop if pngs

  
} # ends loop countries

lu(allcountries_RLI$country)
lu(allcountries_CRLI$country)


#5.5 to save csv and pdf with the results of all cnts together

## single csv with all cnts 
finfolderF=paste(finfolder, "/", iso2use, "/csv_tables", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)

rliallcntsfile=paste(finfolderF,"/", iso2use, "_", "allcountries_RLI_",name2save,".csv", sep="")
rliallcntsfile
#if (writefiles) write.csv(allcountries_RLI,rliallcntsfile, row.names=F)

crliallcntsfile=paste(finfolderF,"/", iso2use, "_", "allcountries_CRLI_",name2save,".csv", sep="")
crliallcntsfile
#if (writefiles) write.csv(allcountries_CRLI,crliallcntsfile, row.names=F)



###### single pdf with all plots
finfolderF=paste(finfolder, "/", iso2use, "/pdfs", sep="")
  finfolderF
    if (dir.exists(finfolderF)==F) dir.create(finfolderF,recursive = T)


pdfallname=paste(finfolderF,"/",  iso2use, "_", "RLI_all_countries_",name2save,".pdf", sep="")
pdfallname
pdf(file=pdfallname, width = 12, height = 10)
par(las=1,cex.axis=1.2, cex.lab=1.3, cex.main=1.5, mar=c(5,5,4,2), mgp=c(3.5, 1, 0))

cts=unique(allcountries_RLI$country)
for (i in 1:length(cts))    #length(cts)
{
  #i=1
  #i=which(cts=="ABW")
  i
  
  ct1=as.character(cts[i])
  ct1
  rli2=rliextf[rliextf$country==ct1,]
  
  namect1=as.character(ct1)
  if (iso2use=="ISO_BL") namect1=paste(isos$uname_BL[isos$isof==as.character(ct1)][1] ," (", ct1, ")", sep="")
  if (iso2use=="ISO_SDG") namect1=paste(isos$uname_SDG[isos$isof==as.character(ct1)][1] ," (", ct1, ")", sep="")
  namect1
  
  fspct=pastables[pastables$Scientific%in%unique(tbf2$Scientific[tbf2$isof==ct1]),]
  head(fspct)
  respc=with(fspct, aggregate(year, list(iucn=iucn, year=year,  group=group), length))
  
  rlinew=allcountries_RLI[allcountries_RLI$country==ct1,]
  
  
  #graph all
  miny=.15
  topy=min(c(1.02, max(rlinew$qn95)*1.1))
  topy
  basy=min(rlinew$qn05)*.99
  difrgy=diff(range(basy, topy))
  if (difrgy<miny) {
    basy=topy-miny
    difrgy=diff(range(basy, topy))}
  with(rlinew,plot(year,rli, ylim=c(basy*.99,topy), xlim=c(min(year), max(year)+1), type="n", xlab="Year", yaxt="n", ylab="Red List Index of species survival", main=namect1))
  if (difrgy<0.3) axis(2, seq(0,1, by=.05))
  if (difrgy>0.3) axis(2, seq(0,1, by=.1))
  gs=unique(rlinew$group[rlinew$group!="aggregated"])
  lu(gs)
  for (g in 1:length(gs))
  {
    #g=1
    gs1=as.character(gs[g])
    res2=rlinew[rlinew$group==gs1,]
    with(res2,polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
  }
  with(rlinew[rlinew$group=="aggregated",], polygon(c(year,rev(year)),c(qn05,rev(qn95)), col=cinz, border=cinz))
  for (g in 1:length(gs))
  {
    gs1=as.character(gs[g])
    res2=rlinew[rlinew$group==gs1,]
    with(res2,lines(year,rli, col=2, lwd=2))
    text(max(res2$year)+0.5, res2$rli[nrow(res2)],paste(gs1,"s", sep=""), cex=1, adj=c(0,0.5))
  }
  with(rlinew[rlinew$group=="aggregated",], lines(year,rli, col=4, lwd=2))
  
}
dev.off()

} # ends loop if length(cts)>0 


} ## ends the loop to go through all the ccombinations if isos
} ###  ends loop of length(isos2use)>0
} ## ends loop of k=1 | dis_subglobal

} ####### ends loop to go through all the disaggregations


#PART 6. ORGANIZE FINAL FILE FOLLOWING THE UN TEMPLATE ######################################################################
############################################################################################################################################


if (save_UN_template)
{
lu=function (x=x) length(unique(x))   ## function to estimate the number of unique values in a vector # repeated here to when this part is run independently
finfolder=paste(finfolderP,"General RLI", sep="/")
finfolder
tnames=data.frame(varis=paste("V",1:16, sep=""), 
  fnames=c("Indicator",	"SeriesID",	"SeriesDescription",	"GeoAreaCode","GeoAreaName", "TimePeriod","Value","Time_Detail",	
  "UpperBound","LowerBound","Source", "FootNote","Nature" ,"Units","Reporting Type","SeriesCode"))
  
 tnames

table_sdg=data.frame()

#global
finfolderF=paste(finfolder, "/", "global/csv_tables", sep="")
finalname=paste(finfolderF, "/", "global_aggregated.csv", sep="")
tab2r=read.csv(finalname)


tabint=data.frame(V6=tab2r$year, V7=tab2r$rli, V8=tab2r$year, V9=tab2r$qn95, V10=tab2r$qn05)
head(tabint,20)

tabint$V1="15.5.1"
tabint$V2="2840"
tabint$V3="Red List Index"
tabint$V4=1
tabint$V5="World"

tabint$V11="BirdLife International and IUCN (2020), based on global estimates of the extinction risk (IUCN Red List categories) of all mammals, birds, amphibians, corals and cycads, derived from local and national data, disaggregated to the national scale and weighted by the proportion of each species's distribution in the country or region." 
tabint$V12=""
tabint$V13="E"
tabint$V14="INDEX"
tabint$V15="G"
tabint$V16="ER_RSK_LSTI"


tabintF=tabint[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")]
head(tabintF)


#names(tabintF)=tnames$fnames
table_sdg=rbind(table_sdg,tabintF) 
head(table_sdg)
tail(table_sdg)

#countries

finfolderF=paste(finfolder, "/", "ISO_SDG/csv_tables", sep="")

fls=dir(finfolderF)
tab2r=data.frame()
for (y in 1:lu(fls)) {
  tab2r=rbind(tab2r, read.csv(paste(finfolderF, fls[y], sep= '/')))
}
tab2r=tab2r[tab2r$group=="aggregated",]
head(tab2r)
tab2r$ISO=tab2r$country
nrow(tab2r)
codes2=codesreg[codesreg$Reference=="1.0-Global",]

tab2r=merge(tab2r, codes2)
tab2r=tab2r[order(tab2r$year),]
tab2r=tab2r[order(tab2r$country),]
nrow(tab2r)
head(tab2r)


tabint=data.frame(V4=tab2r$M49Code, V5=tab2r$CountryName, V6=tab2r$year, V7=tab2r$rli, V8=tab2r$year, V9=tab2r$qn95, V10=tab2r$qn05, iso=tab2r$ISO)
head(tabint,20)

tabint$V1="15.5.1"
tabint$V2="2840"
tabint$V3="Red List Index"

tabint$V11="BirdLife International and IUCN (2020), based on global estimates of the extinction risk (IUCN Red List categories) of all mammals, birds, amphibians, corals and cycads, derived from local and national data, disaggregated to the national scale and weighted by the proportion of each species's distribution in the country or region." 
tabint$V12=""
tabint$V13="E"
tabint$V14="INDEX"
tabint$V15="G"
tabint$V16="ER_RSK_LSTI"

tabintF=tabint[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")]
head(tabintF)

#names(tabintF)=tnames$fnames
table_sdg=rbind(table_sdg,tabintF) 
head(table_sdg)
tail(table_sdg)


#regional

finfolderF=paste(finfolder, "/", "SDG_Region/csv_tables", sep="")
fls=dir(finfolderF)
tab2r=data.frame()
for (y in 1:lu(fls)) {
  tab2r=rbind(tab2r, read.csv(paste(finfolderF, fls[y], sep= '/')))
}
finfolderF=paste(finfolder, "/", "SDG_Subregion/csv_tables", sep="")
fls=dir(finfolderF)
for (y in 1:lu(fls)) {
  tab2r=rbind(tab2r, read.csv(paste(finfolderF, fls[y], sep= '/')))
}

tab2r=tab2r[tab2r$group=="aggregated",]
head(tab2r)
tab2r$ISO=tab2r$country
nrow(tab2r)
codes2=codesreg[codesreg$Reference=="2.1-Regional (SDG)",]

tab2r=merge(tab2r, codes2)
nrow(tab2r)
head(tab2r)

tabint=data.frame(V4=tab2r$M49Code, V5=tab2r$CountryName, V6=tab2r$year, V7=tab2r$rli, V8=tab2r$year, V9=tab2r$qn95, V10=tab2r$qn05)
head(tabint,20)

tabint$V1="15.5.1"
tabint$V2="2840"
tabint$V3="Red List Index"

tabint$V11="BirdLife International and IUCN (2020), based on global estimates of the extinction risk (IUCN Red List categories) of all mammals, birds, amphibians, corals and cycads, derived from local and national data, disaggregated to the national scale and weighted by the proportion of each species's distribution in the country or region." 
tabint$V12=""
tabint$V13="E"
tabint$V14="INDEX"
tabint$V15="G"
tabint$V16="ER_RSK_LSTI"

tabintF=tabint[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")]
head(tabintF)

#names(tabintF)=tnames$fnames
table_sdg=rbind(table_sdg,tabintF) 
head(table_sdg)
tail(table_sdg)



#other groups

finfolderF=paste(finfolder, "/", "LLDC_SIDS/csv_tables", sep="")
fls=dir(finfolderF)
tab2r=data.frame()
for (y in 1:lu(fls)) {
  tab2r=rbind(tab2r, read.csv(paste(finfolderF, fls[y], sep= '/')))
}
finfolderF=paste(finfolder, "/", "LDC/csv_tables", sep="")
fls=dir(finfolderF)
for (y in 1:lu(fls)) {
  tab2r=rbind(tab2r, read.csv(paste(finfolderF, fls[y], sep= '/')))
}

tab2r=tab2r[tab2r$group=="aggregated",]
head(tab2r)
tab2r$ISO=tab2r$country
nrow(tab2r)
codes2=codesreg[codesreg$Reference=="2.3-Other groupings",]

tab2r=merge(tab2r, codes2)
nrow(tab2r)
head(tab2r)

tabint=data.frame(V4=tab2r$M49Code, V5=tab2r$CountryName, V6=tab2r$year, V7=tab2r$rli, V8=tab2r$year, V9=tab2r$qn95, V10=tab2r$qn05)
head(tabint,20)

tabint$V1="15.5.1"
tabint$V2="2840"
tabint$V3="Red List Index"

tabint$V11="BirdLife International and IUCN (2020), based on global estimates of the extinction risk (IUCN Red List categories) of all mammals, birds, amphibians, corals and cycads, derived from local and national data, disaggregated to the national scale and weighted by the proportion of each species's distribution in the country or region." 
tabint$V12=""
tabint$V13="E"
tabint$V14="INDEX"
tabint$V15="G"
tabint$V16="ER_RSK_LSTI"

tabintF=tabint[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")]
head(tabintF)

table_sdg=rbind(table_sdg,tabintF) 
head(table_sdg)
tail(table_sdg)

table_sdg=table_sdg[table_sdg$V8>1992,]

### change headers and save
names(table_sdg)=tnames$fnames
head(table_sdg)
write.csv(table_sdg, paste(finfolderP, name_UN_template, sep="/"), row.names=F)
}   # ends if save_UN_template
