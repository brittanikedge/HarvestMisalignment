all_points_bad <- readRDS("~/Box/Machine_Misalignment/Results/all_points_map_bad.rds")
all_points_bad
ggsave("~/Box/Machine_Misalignment/Results/all_points_map_bad.jpeg")

all_points_good <-readRDS("~/Box/Machine_Misalignment/Results/all_points_map_good.rds")
all_points_good
ggsave("~/Box/Machine_Misalignment/Results/all_points_map_good.jpeg")

alignment_bad <- readRDS("~/Box/Machine_Misalignment/Results/map_alignment_bad.rds")
alignment_bad
ggsave("~/Box/Machine_Misalignment/Results/map_alignment_bad.jpeg")

alignment_good <- readRDS("~/Box/Machine_Misalignment/Results/map_alignment_good.rds")
alignment_good
ggsave("~/Box/Machine_Misalignment/Results/map_alignment_good.jpeg")
