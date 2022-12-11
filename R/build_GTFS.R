# Built GTFS
remotes::install_github("itsleeds/uk2gtfs")
library(UK2GTFS)

# RAIL
gtfs <- atoc2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/ATOC/timetable/2021-10-09/ttis152.zip",
                  ncores = 20,
                  transfers = FALSE)
gtfs_validate_internal(gtfs)

stops <- gtfs$stops
stop_times <- gtfs$stop_times

foo = stop_times[!stop_times$stop_id %in% stops$stop_id,]
bar = unique(foo$stop_id)

gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_id %in% gtfs$stops$stop_id,]

gtfs_write(gtfs, "data/GTFS", name = "rail_20211009")


# TransXchange

cal <- get_bank_holidays()
naptan <- get_naptan()

# EA
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/EA.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "EA_20211012")

#EM
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/EM.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "EM_20211012")


#L
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/L.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "L_20211012")

#NE
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/NE.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "NE_20211012")

#NW
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/NW.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "NW_20211012")

#s
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/S.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "S_20211012")


#sE - "" produced in agency_id
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/SE.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "SE_20211012")


#sW
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/SW.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "SW_20211012")


#W Unknown agency_id in routes
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/W.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "W_20211012")

#WM
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/WM.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "WM_20211012")


#Y
gtfs <- transxchange2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXChange/data_20211012/Y.zip", 
                          silent = FALSE, 
                          cal = cal,
                          naptan = naptan, 
                          force_merge = TRUE,
                          ncores = 20, try_mode = FALSE)
gtfs <- gtfs_clean(gtfs)
gtfs <- gtfs_interpolate_times(gtfs, 20)
gtfs_validate_internal(gtfs)
gtfs_write(gtfs, "data/GTFS", name = "Y_20211012")


foo = UK2GTFS:::gtfs_stops_sf(gtfs)

