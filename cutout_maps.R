library(magrittr)
library(sf)
library(ggplot2)
library(tictoc)
library(dplyr)
library(gridExtra)

# Please see ReadMe for required shapefiles.

segment_file <- "candidate_segments_0814.csv"

not_segment <- "not_segments.csv"

segment_data <- readr::read_csv(segment_file, guess_max = 3000)
not_segment_data <- readr::read_csv(not_segment, guess_max = 3000)

segment_combined <- rbind(segment_data, not_segment_data)

combined_data <- segment_combined %>%
		dplyr::mutate(
							 gidtr = stringr::str_pad(gidtr, width = 11, pad = "0")
					 ) %>%
		dplyr::group_by(gidtr) %>%
		dplyr::mutate(
							 tract_num = 1:n()
					 ) %>%
		dplyr::filter(
							 tract_num == 1
					 ) %>%
		dplyr::select(
							 gidtr,
							 k8,
							 k7,
							 w8
					 ) %>%
		dplyr::mutate(
							 k7 = dplyr::recode(
															 k7,
															 `1` = 7,
															 `2` = 4,
															 `3` = 8,
															 `4` = 6,
															 `5` = 5,
															 `6` = 2,
															 `7` = 1,
															 `8` = 9,
															 `9` = 10
													 ) %>%
							factor(levels = 1:10),
							 k8 = dplyr::recode(
															 k8,
															 `1` = 7,
															 `2` = 3,
															 `3` = 4,
															 `4` = 8,
															 `5` = 1,
															 `6` = 5,
															 `7` = 2,
															 `8` = 6,
															 `9` = 9,
															 `10` = 10
													 )%>%
							factor(levels = 1:10),
							 w8 = dplyr::recode(
															 w8,
															 `1` = 3,
															 `2` = 2,
															 `3` = 1,
															 `4` = 4,
															 `5` = 6,
															 `6` = 8,
															 `7` = 5,
															 `8` = 7,
															 `9` = 9,
															 `10` = 10
															 )%>%
							factor(levels = 1:10)
					 ) %>%
		dplyr::rename(
							 a = k8,
							 b = k7,
							 c = w8
					 )

shp_dir <- "C:/Users/hshang/Downloads/tract shape file/tract shape file"

tract_base <- read_sf(dsn =shp_dir, layer = "USA") %>%
		dplyr::rename(
							 state = STATEFP,
							 county = COUNTYFP,
							 tract = TRACTCE
					 ) %>%
		dplyr::mutate(
							 state = as.character(state),
							 county = as.character(county),
							 tract = as.character(tract)
					 ) %>%
		dplyr::filter(
							 state %in% stringr::str_pad(as.character(c(seq(1,56), 72)), width = 2, pad = "0")
					 ) %>%
		tidyr::unite(
							 col = "gidtr",
							 state,
							 county,
							 tract,
							 sep  = ""
					 )

national_base <- tract_base %>%
		dplyr::mutate(
							 state = stringr::str_sub(gidtr, 1 , 2)
					 ) %>%
		dplyr::group_by(state) %>%
		dplyr::summarize() %>%
		dplyr::mutate(
							 a = NA_integer_,
							 b = NA_integer_,
							 c = NA_integer_,
							 gidtr = NA_character_
					 ) %>%
		dplyr::ungroup()

map_data <- tract_base %>%
		dplyr::left_join(
							 combined_data,
							 by = "gidtr"
					 ) %>%
		dplyr::mutate(
							 state = stringr::str_sub(gidtr, 1 , 2)
					 ) %>%
		dplyr::select(state, gidtr, a, b, c, geometry) %>%
		rbind(
				national_base
				)




crs_lower48 <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"



seg_fill_pal <- c(
`1` = "#42C17C",
`2` = "#DF444C",
`3` = "#FF8947",
`4` = "#FECE00",
`5` = "#6F441A",
`6` = "#B9BEC1",
`7` = "#836ab3",
`8` = "#17A3BC",
# `10` = "#FFA7BD",
`9` = "#fdc3bc"
)

seg_names <- c(
"Responsive Suburbia",
"Main Street Middle",
"Country Roads",
"Downtown Dynamic",
"Dorms and Barracks",
"Sparse Spaces",
"Ethnic Enclaves",
"Disadvantaged and Low Response",
		"No Segment Assigned "
		)



cities <- list(Chicago = c(-88.132324,41.450961,-87.250671,42.136932), Houston = c(-95.927124,29.005738,-94.424744,30.185496), Iowa = c(-96.745605,40.538852,-89.967041,43.715535), Pittsburgh = c(-80.108871,40.338694,-79.846573,40.521107), SanFran = c(-122.676086,37.468319,-122.012787,38.040520), New_York = c(-74.264832,40.538330,-73.683243,40.888601), Colorado_Springs = c(-104.956512,38.696229,-104.581604,39.044786), DC = c(-77.133636,38.795303,-76.898117,38.999976), Central_Valley = c(-121.470337,34.773204,-118.707275,37.365791), San_Antonio = c(-99.624023,25.661333,-96.855469,29.850173), Atlanta = c(-84.651031,33.342002,-84.112701,33.969559), Richmond = c(-77.763977,37.276238,-77.122650,37.728366), Memphis = c(-91.153564,33.865854,-89.648438,35.554574))

make_cutout <- function(bound_vector){

	names(bound_vector) <- c("xmin", "ymin", "xmax", "ymax")

	lower48_data <- map_data %>%
	dplyr::filter(!state %in% c("02", "15", "72")) %>% st_crop(y = bound_vector)


	bb <- st_bbox(lower48_data)

	bbox_to_df <- function(bbox_object){
		unlist(data.frame(t(data.frame(unclass((bbox_object))))))
	}

	place_geometry <- function(geometry, position, scale = 1) {
		(geometry - st_centroid(st_union(geometry))) * scale +
		st_sfc(st_point(position))
	}



	calculate_range <- function(bb_df){
		names(bb_df) <- NULL
		c(x_range = (bb_df[3] - bb_df[1]), y_range = bb_df[4] - bb_df[2])
	}

	lower_48_bb <- bbox_to_df(st_bbox(lower48_data))





# plot alaska dummy data



combined_data <- list(lower48_data = lower48_data)

recode_segments<- function(data){

	data$a <- data$a %>% recode("10" = "9")

	data$a <- dplyr::if_else(
		is.na(data$gidtr),
		data$a,
		data$a %>%
		tidyr::replace_na("9") %>%
		factor(
			levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")
			)
		)

	return(data)
}

combined_list <- sapply(combined_data, recode_segments, simplify = FALSE, USE.NAMES = TRUE)




final_a <- ggplot() + geom_sf(data = combined_list$lower48_data, aes(fill = a, color = is.na(gidtr)), lwd = 0) + scale_fill_manual(values = seg_fill_pal, labels = seg_names, name = "", drop = FALSE) +
scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "transparent"), guide = "none") +
ggspatial::annotation_scale(unit_category = "imperial", pad_x = unit(.35, "npc"), pad_y = unit(.010, "npc"), style = "ticks", height = unit(.02, "npc"), text_cex = 1.5, text_face = 2, line_width = 2) +
ggthemes::theme_map() +
theme(
	panel.grid.major = element_line(colour="transparent"),
	plot.title = element_text(size = 20, face = "bold"),
	legend.key = element_rect(color = "black"),  legend.position = "none")+
coord_sf(xlim = c(lower_48_bb[1], lower_48_bb[3]), ylim = c(lower_48_bb[2], lower_48_bb[4]))

return(final_a)

}

cutout_list <- sapply(cities, make_cutout, USE.NAMES = TRUE, simplify = FALSE)

save_path <- "C:/Users/hshang/Downloads/tract shape file/tract shape file/cutouts/"

save_cutout <- function(i, path, w = 20, h = 10){
	ggsave(filename = paste(paste(path, names(cutout_list)[i], sep = ""), ".png", sep = ""), plot = cutout_list[[i]], width = 20, height = 10, units = "in")
}

lapply(seq_along(cutout_list), save_cutout, path = save_path)




