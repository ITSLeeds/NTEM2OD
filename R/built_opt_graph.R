remotes::install_github("ropensci/opentripplanner")
library(opentripplanner)

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

# log1 = otp_build_graph(path_opt,
#                        path_data,
#                        memory = 80000,
#                        router = "great-britain-NTEM",
#                        quiet = FALSE)

log2 = otp_setup(path_opt,
                 path_data,
                 memory = 80000,
                 router = "great-britain-NTEM",
                 quiet = FALSE,
                 securePort = 8082, 
                 pointsets = TRUE,
                 analyst = TRUE)

#java -Xmx100000M -d64 -jar "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar" --router great-britain-NTEM --graphs "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs" --server --port 8080 --securePort 8082 --analyst --pointSets "D:/OneDrive - University of Leeds/Data/opentripplanner/pointsets"