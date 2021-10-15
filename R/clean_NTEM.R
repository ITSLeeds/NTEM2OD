# Read in NTEM Data


read_tempro_sheet <- function(file, mode_type){
  
  mode_table = data.frame(mode = c("walk","cycle","drive","passenger","bus","rail","combined"),
                          mode_name = c("Walk","Cycle","Car Driver","Car Passenger","BusCoach","RailUnderground","Combined Modes"))
  mode_name = mode_table$mode_name[mode_table$mode == mode_type]
  nms <- c("msoa",
           "msoa_name",
           "HB_work_origin",
           "HB_work_destination",
           "HB_business_origin",
           "HB_business_destination",
           "HB_education_origin",
           "HB_education_destination",
           "HB_shopping_origin",
           "HB_shopping_destination",
           "HB_personal_origin",
           "HB_personal_destination",
           "HB_recreation_origin",
           "HB_recreation_destination",
           "HB_friends_origin",
           "HB_friends_destination",
           "HB_holiday_origin",
           "HB_holiday_destination",
           "NHB_work_origin",
           "NHB_work_destination",
           "NHB_business_origin",
           "NHB_business_destination",
           "NHB_education_origin",
           "NHB_education_destination",
           "NHB_shopping_origin",
           "NHB_shopping_destination",
           "NHB_personal_origin",
           "NHB_personal_destination",
           "NHB_recreation_origin",
           "NHB_recreation_destination",
           "NHB_holiday_origin",
           "NHB_holiday_destination")
  
  mode <- readxl::read_excel(file, sheet = mode_name, col_names = nms)
  #names(mode) <- nms
  mode$mode <- mode_type
  mode <- mode[3:nrow(mode),]

  
  # Find the baseline data
  by_start <- seq_len(nrow(mode))[mode$msoa == "Base Year"]
  by_end <- seq_len(nrow(mode))[mode$msoa == "Future Year"]
  
  by_start <- by_start[!is.na(by_start)]
  by_end <- by_end[!is.na(by_end)]
  
  by_start <- by_start + 3
  by_end <- by_end - 2
  
  mode <- mode[by_start:by_end,]
  mode <- mode[!mode$msoa %in% c("Region","County","Authority"),]
  mode[3:32] <- lapply(mode[3:32], as.integer)
  
  return(mode)
  
}


read_tempro_file <- function(file){
  
  walk <- read_tempro_sheet(file, "walk")
  cycle <- read_tempro_sheet(file, "cycle")
  drive <- read_tempro_sheet(file, "drive")
  passenger <- read_tempro_sheet(file, "passenger")
  bus <- read_tempro_sheet(file, "bus")
  rail <- read_tempro_sheet(file, "rail")
  combined <- read_tempro_sheet(file, "combined")
  
  all <- dplyr::bind_rows(list(walk,cycle,drive,passenger,bus,rail,combined))
  all <- all[,c("msoa","msoa_name","mode","HB_work_origin",
                "HB_work_destination","HB_business_origin","HB_business_destination",
                "HB_education_origin","HB_education_destination","HB_shopping_origin",
                "HB_shopping_destination","HB_personal_origin","HB_personal_destination",
                "HB_recreation_origin","HB_recreation_destination","HB_friends_origin",
                "HB_friends_destination","HB_holiday_origin","HB_holiday_destination",
                "NHB_work_origin","NHB_work_destination","NHB_business_origin",
                "NHB_business_destination","NHB_education_origin","NHB_education_destination",
                "NHB_shopping_origin","NHB_shopping_destination","NHB_personal_origin",
                "NHB_personal_destination","NHB_recreation_origin","NHB_recreation_destination",
                "NHB_holiday_origin","NHB_holiday_destination" )]
  all <- all[order(all$msoa),]
  return(all)
}

E <- read_tempro_file("data/TEMPRO/E-OD-Day.xlsx")
EM <- read_tempro_file("data/TEMPRO/EM-OD-Day.xlsx")
L <- read_tempro_file("data/TEMPRO/L-OD-Day.xlsx")
NE <- read_tempro_file("data/TEMPRO/NE-OD-Day.xlsx")
NW <- read_tempro_file("data/TEMPRO/NW-OD-Day.xlsx")
S <- read_tempro_file("data/TEMPRO/S-OD-Day.xlsx")
SE <- read_tempro_file("data/TEMPRO/SE-OD-Day.xlsx")
SW <- read_tempro_file("data/TEMPRO/SW-OD-Day.xlsx")
W <- read_tempro_file("data/TEMPRO/W-OD-Day.xlsx")
WM <- read_tempro_file("data/TEMPRO/WM-OD-Day.xlsx")
Y <- read_tempro_file("data/TEMPRO/Y-OD-Day.xlsx")

GB <- dplyr::bind_rows(list(E,EM,L,NE,NW,S,SE,SW,W,WM,Y))
rm(E,EM,L,NE,NW,S,SE,SW,W,WM,Y)

saveRDS(GB,"data/TEMPRO/GB-OA-Day-Baseline.Rds")

 

