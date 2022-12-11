library(sf)
library(tmap)
tmap_mode("plot")

od <- read_sf("data/NTEM/NTEM_desire_lines.geojson")
# quantile(od$all, probs = seq(0,1,0.01))
# foo <- od[od$walk > quantile(od$walk, probs = 0.99) |
#           od$bus > quantile(od$bus, probs = 0.99) |
#           od$drive > quantile(od$drive, probs = 0.99) |
#           od$passenger > quantile(od$passenger, probs = 0.99) |
#           od$rail > quantile(od$rail, probs = 0.99) |
#           od$walk > quantile(od$walk, probs = 0.99),]

# m = tm_shape(foo) +
#   tm_lines(lwd = c("passenger","bus","drive","passenger","rail","walk"),
#            col = c("passenger","bus","drive","passenger","rail","walk"),
#            palette = "OrRd",
#            legend.col.show = FALSE,
#            legend.lwd.show = FALSE,
#            style = "quantile",
#            n = 10) +
#   tm_facets(sync = TRUE, ncol = 3) +
#   tm_layout(panel.labels = c("Bipassenger","Bus/Coach","Car driver",
#                              "Car passenger","Rail","Walk"))

m_cycle = tm_shape(od[od$cycle > quantile(od$cycle, probs = 0.99),]) +
  tm_lines(lwd = c("cycle"),
           col = c("cycle"),
           palette = "OrRd",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           style = "quantile",
           n = 10) +
  tm_layout(title = "Bicycle")

m_bus = tm_shape(od[od$bus > quantile(od$bus, probs = 0.99),]) +
  tm_lines(lwd = c("bus"),
           col = c("bus"),
           palette = "OrRd",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           style = "quantile",
           n = 10) +
  tm_layout(title = "Bus")

m_drive = tm_shape(od[od$drive > quantile(od$drive, probs = 0.99),]) +
  tm_lines(lwd = c("drive"),
           col = c("drive"),
           palette = "OrRd",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           style = "quantile",
           n = 10) +
  tm_layout(title = "Car driver")

m_passenger = tm_shape(od[od$passenger > quantile(od$passenger, probs = 0.99),]) +
  tm_lines(lwd = c("passenger"),
           col = c("passenger"),
           palette = "OrRd",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           style = "quantile",
           n = 10) +
  tm_layout(title = "Car passenger")

m_rail = tm_shape(od[od$rail > quantile(od$rail, probs = 0.99),]) +
  tm_lines(lwd = c("rail"),
           col = c("rail"),
           palette = "OrRd",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           style = "quantile",
           n = 10) +
  tm_layout(title = "Rail")

m_walk = tm_shape(od[od$walk > quantile(od$walk, probs = 0.99),]) +
  tm_lines(lwd = c("walk"),
           col = c("walk"),
           palette = "OrRd",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           style = "quantile",
           n = 10) +
  tm_layout(title = "Walk")




m_all = tmap_arrange(m_cycle, m_bus, m_drive, m_passenger, m_rail, m_walk, ncol = 3)
tmap_save(m_all, filename = "plots/desire_lines3.jpg", width = 5.6, height = 7)

tmap_save(m_all, filename = "plots/desire_lines_RGS.jpg", width = 5.6, height = 7, dpi = 600)

quantile(od$all, probs = 0.99)
