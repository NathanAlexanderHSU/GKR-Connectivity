
#######################################################
####CIRCUIT SCAPE######
######Load Cost maps and occurrences into Arc, and export again using the circuitscape plug in

###FOR INDIVIDUAL PAIRWISE DISTANCES, USE TEXT FILE INSTEAD OF RASTER
	##Convert GKR location to ascii in ARCMAP
	##Export as comma delimited with Object ID (MUST BE NUMERIC)
	##open in Excel: Excel Processes:
		##Delete Header
		##Move ID to 1st column
		##SAVE AS TAB DELIMITED
	##For individual in circuitscape, must tell it to write the CUMULATIVE CURRENT MAP to do
		##all pairwise functions.  otherwise will give an "nan" as a pairwise cost distance


##########################################################
######New Solution-create a TAB DELIMITED file of Site Locations.  Will Populate individual 
		##distance matrices by resampling the distance matrices of sites.  This will
		##speed up processing time (CS will measure 46 pairwise distances rather than 121)


###File locations need to be set for different computers

######CircuitScape
rm(list=ls())

# Make a place holder for the cs_run.exe
CS_exe <- 'C:\\"Program Files"\\Circuitscape\\cs_run.exe' # Don't forget the "Program Files" problem

setwd("H:\\THESISDATA")


##Read in All of the cost maps previously created
maps <- list.files(path="H:\\THESISDATA\\OutputCostMaps")    #prompt user for dir containing raster files
maps


num_files<-length(maps)
num_files
Sites<-read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Site.txt")

memory.limit(size=10000)

# Make an .ini file
for(i in 1:num_files) {
CS_ini<-c("[Options for advanced mode]",
"ground_file_is_resistances = True",
"remove_src_or_gnd = keepall",
"ground_file = (Browse for a ground point file)",
"use_unit_currents = False",
"source_file = (Browse for a current source file)",
"use_direct_grounds = False",

"[Mask file]",
"mask_file = None",
"use_mask = False",

"[Calculation options]",
"low_memory_mode = False",
"parallelize = False",
"solver = cg+amg",
"print_timings = False",
"preemptive_memory_release = False",
"print_rusages = False",
"max_parallel = 0",

"[Short circuit regions (aka polygons)]",
"polygon_file = (Browse for a short-circuit region file)",
"use_polygons = False",

"[Options for one-to-all and all-to-one modes]",
"use_variable_source_strengths = False",
"variable_source_file = None",

"[Output options]",
"set_null_currents_to_nodata = False",
"set_focal_node_currents_to_zero = False",
"set_null_voltages_to_nodata = False",
"compress_grids = False",
"write_cur_maps = False",
"write_volt_maps = False",
paste("output_file = H:\\THESISDATA\\CSoutput\\",maps[i],".out",collapse='',sep=""),
"write_cum_cur_map_only = False",
"log_transform_maps = False",
"write_max_cur_maps = False",

"[Version]",
"version = 4.0.5",

"[Options for reclassification of habitat data]",
"reclass_file = (Browse for file with reclassification data)",
"use_reclass_table = False",

"[Logging Options]",
"log_level = INFO",
paste("log_file = H:\\THESISDATA\\CSoutput\\",maps[i],".log",collapse='',sep=""),
"profiler_log_file = None",
"screenprint_log = False",

"[Options for pairwise and one-to-all and all-to-one modes]",
"included_pairs_file = (Browse for a file with pairs to include or exclude)",
"use_included_pairs = False",
"point_file = H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Site.txt",

"[Connection scheme for raster habitat data]",
"connect_using_avg_resistances = False",
"connect_four_neighbors_only = False",

"[Habitat raster or graph]",
"habitat_map_is_resistances = True",
paste("habitat_file = H:\\THESISDATA\\OutputCostMaps\\",maps[i],collapse='',sep=""),

"[Circuitscape mode]",
"data_type = raster",
"scenario = pairwise")


# Write it to your working directory
writeLines(CS_ini,"myini.ini")

# Make the CS run cmd
CS_run <- paste(CS_exe, paste(getwd(),"myini.ini",sep="/")) # Make the cmd

# Run the command
system(CS_run)

 print(paste(i, " of ", length(num_files), ": ", Sys.time(), sep=""))
}
}
# Import the effective resistance


for(i in 1:num_files) {
rdist <- as.matrix(as.dist(read.csv(paste("H:\\THESISDATA\\CSoutput\\",maps[i],"_resistances.out",sep=""),sep=" ",row.names=1,header=1)))
write.csv(rdist, paste("H:\\THESISDATA\\IBRDistMatrices\\",maps[i],".csv",collapse='',sep=""))
}


###For Habitat Suitability analysis
rdist <- as.matrix(as.dist(read.csv(paste("H:\\THESISDATA\\CSoutput\\HabSuitabilityConnectivity_resistances.out",sep=""),sep=" ",row.names=1,header=1)))
write.csv(rdist, paste("H:\\THESISDATA\\IBRDistMatrices\\HabSuitabilityConnectivity.csv"))
