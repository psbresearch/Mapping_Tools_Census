library(magrittr)
library(sf)
library(ggplot2)
library(tictoc)
library(dplyr)
library(gridExtra)
require(extrafont)

font_import(paths = "C:/Windows/Fonts", pattern = "Calibri")



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



# saveRDS(map_data, "map_data.RDS")

# map_data <- readRDS("map_data.RDS")


crs_lower48 <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

crs_alaska <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "

crs_hawaii <- "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

crs_pr <- "+proj=lcc +lat_1=18.43333333333333 +lat_2=18.03333333333333 +lat_0=17.83333333333333 +lon_0=-66.43333333333334 +x_0=152400.3048006096 +y_0=0 +ellps=clrk66 +towgs84=11,72,-101,0,0,0,0 +units=us-ft +no_defs"


lower48_data <- map_data %>%
    dplyr::filter(!state %in% c("02", "15", "72"))


lower48_data <- st_transform(
    lower48_data,
    crs_lower48
    )

bb <- st_bbox(lower48_data)

bbox_to_df <- function(bbox_object){
  unlist(data.frame(t(data.frame(unclass((bbox_object))))))
}

place_geometry <- function(geometry, position, scale = 1) {
  (geometry - st_centroid(st_union(geometry))) * scale +
    st_sfc(st_point(position))
}


alaska_data <- map_data %>%
    dplyr::filter(state == "02") %>%
    st_transform(crs_alaska)




st_geometry(alaska_data) <- place_geometry(
  st_geometry(alaska_data),
  c(bb$xmin + 0.08*(bb$xmax - bb$xmin),
    bb$ymin + 0.2*(bb$ymax - bb$ymin)),
  scale = 0.6
)
st_crs(alaska_data) <- crs_alaska

# crop weird hawaii


hawaii_data <- map_data %>%
    dplyr::filter(state == "15") %>% st_crop(xmin = -160.3404426, ymin = 18.86546, xmax = -154.7558, ymax = 22.05703) %>%
    st_transform(crs_hawaii)

st_geometry(hawaii_data) <- place_geometry(
  st_geometry(hawaii_data),
  c(bb$xmin + 0.3*(bb$xmax - bb$xmin),
    bb$ymin + 0.1*(bb$ymax - bb$ymin)), scale = 2.5
)
st_crs(hawaii_data) <- crs_hawaii

pr_data <- map_data %>%
    dplyr::filter(state == "72") %>%
    st_transform(crs_pr)

st_geometry(pr_data) <- place_geometry(
  st_geometry(pr_data),
  c(bb$xmin + (bb$xmax - bb$xmin),
    bb$ymin + (bb$ymax - bb$ymin)), scale = 2.5)
st_crs(pr_data) <- crs_pr




# final_data <- rbind(lower48_data, alaska_data, hawaii_data)

alaska_bb <- bbox_to_df(st_bbox(alaska_data))
lower_48_bb <- bbox_to_df(st_bbox(lower48_data))
hawaii_bb <- bbox_to_df(st_bbox(hawaii_data))
pr_bb <- bbox_to_df(st_bbox(pr_data))


calculate_range <- function(bb_df){
  names(bb_df) <- NULL
  c(x_range = (bb_df[3] - bb_df[1]), y_range = bb_df[4] - bb_df[2])
}



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
"Student and Military Communities",
"Sparse Spaces",
"Ethnic Enclaves",
"Disadvantaged and Low Response",
    "No ACS Mailout"
    )


# plot alaska dummy data

alaska_dummy <- alaska_data

alaska_dummy$a <- alaska_dummy$a %>% recode("10" = "9")


alaska_dummy$a <- alaska_dummy$a %>% tidyr::replace_na("9") %>% factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8","9"))

alaska_dummy <- alaska_dummy %>% filter(!is.na(gidtr))


alaska <- alaska_dummy %>% ggplot(aes(fill = a, color = is.na(gidtr))) +
    geom_sf(lwd = 0) +
    scale_fill_manual(values = seg_fill_pal, name = "",  labels = seg_names, drop = FALSE) +
    scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "transparent"), guide = "none") +
    ggthemes::theme_map() +
    geom_rect(aes(xmin = alaska_bb["xmin"] - .05*(alaska_bb["xmax"] - alaska_bb["xmin"]), ymin = alaska_bb["ymin"] - .05*(alaska_bb["ymax"] - alaska_bb["ymin"]), xmax = alaska_bb["xmax"] + .05*(alaska_bb["xmax"] - alaska_bb["xmin"]), ymax = alaska_bb["ymax"] + .05*(alaska_bb["ymax"] - alaska_bb["ymin"])), alpha = 0, colour = "black",  inherit.aes = FALSE) +
    theme(
        panel.grid.major = element_line(colour="transparent"), legend.position = "right", legend.key.size =  unit(0.5, "in"), legend.text = element_text(size=12, family = "Calibri"), legend.title = element_text(size=16, family = "Calibri"))

    legend <- ggpubr::get_legend(alaska)





combined_data <- list(alaska_data = alaska_data, hawaii_data = hawaii_data, pr_data = pr_data, lower48_data = lower48_data)

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

list2env(combined_list, envir = .GlobalEnv)




final_a <- ggplot() + geom_sf(data = lower48_data, aes(fill = a, color = is.na(gidtr)), lwd = 0) + scale_fill_manual(values = seg_fill_pal, labels = seg_names, name = "", drop = FALSE) +
    scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "transparent"), guide = "none") +

    ggspatial::annotation_scale(unit_category = "imperial", pad_x = unit(.35, "npc"), pad_y = unit(.1, "npc"), style = "ticks", height = unit(.02, "npc"), text_cex = .5) +
     ggthemes::theme_map() +
    theme(
        panel.grid.major = element_line(colour="transparent"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.key = element_rect(color = "black"),  legend.position = "none"
    ) + coord_sf(xlim = c(lower_48_bb[1]*1.5, lower_48_bb[3]*1.5), ylim = c(lower_48_bb[2]*1.5, lower_48_bb[4]*1.75))


    alaska <- ggplotGrob(
        ggplot() +
        geom_sf(data = alaska_data, aes(fill = a, color = is.na(gidtr)), lwd = 0) +
        ggspatial::annotation_scale(unit_category = "imperial", pad_x = unit(.075, "npc"), pad_y = unit(.1, "npc"), style = "ticks", height = unit(.02, "npc"), text_cex = .5) +
        scale_fill_manual(values = seg_fill_pal, name = "", drop = FALSE, guide = "none") +
        scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "transparent"), guide = "none") +
        ggthemes::theme_map() +

       geom_rect(aes(xmin = alaska_bb["xmin"] - .05*(alaska_bb["xmax"] - alaska_bb["xmin"]), ymin = alaska_bb["ymin"] - .2*(alaska_bb["ymax"] - alaska_bb["ymin"]), xmax = alaska_bb["xmax"] + .05*(alaska_bb["xmax"] - alaska_bb["xmin"]), ymax = alaska_bb["ymax"] + .05*(alaska_bb["ymax"] - alaska_bb["ymin"])), alpha = 0, colour = "black",  inherit.aes = FALSE) +
        theme(
            panel.grid.major = element_line(colour="transparent"),
            legend.position = "none"
        )
    )


    hawaii <- ggplotGrob(
        ggplot() +
        geom_sf(data = hawaii_data, aes(fill = a, color = is.na(gidtr)), lwd = 0) +
       ggspatial::annotation_scale(unit_category = "imperial", pad_x = unit(.075, "npc"), pad_y = unit(.15, "npc"), style = "ticks", height = unit(.02, "npc"), text_cex = .5) +
        scale_fill_manual(values = seg_fill_pal, name = "", drop = FALSE, guide = "none") +
        scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "transparent"), guide = "none") +
        ggthemes::theme_map() +

       geom_rect(aes(xmin = hawaii_bb["xmin"] - .05*(hawaii_bb["xmax"] - hawaii_bb["xmin"]), ymin = hawaii_bb["ymin"] - .10*(hawaii_bb["ymax"] - hawaii_bb["ymin"]), xmax = hawaii_bb["xmax"] + .05*(hawaii_bb["xmax"] - hawaii_bb["xmin"]), ymax = hawaii_bb["ymax"] + .05*(hawaii_bb["ymax"] - hawaii_bb["ymin"])), alpha = 0, colour = "black",  inherit.aes = FALSE) +
        theme(
            panel.grid.major = element_line(colour="transparent"),
            legend.position = "none"
        )
    )


    pr <- ggplotGrob(
        ggplot() +
        geom_sf(data = pr_data, aes(fill = a, color = is.na(gidtr)), lwd = 0) +
         ggspatial::annotation_scale(unit_category = "imperial", pad_x = unit(.075, "npc"), pad_y = unit(.15, "npc"), style = "ticks", height = unit(.02, "npc"), text_cex = .5) +
        scale_fill_manual(values = seg_fill_pal, name = "", drop = FALSE, guide = "none") +
        scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "transparent"), guide = "none") +
        ggthemes::theme_map() +

       geom_rect(aes(xmin = pr_bb["xmin"] - .05*(pr_bb["xmax"] - pr_bb["xmin"]), ymin = pr_bb["ymin"] - .45*(pr_bb["ymax"] - pr_bb["ymin"]), xmax = pr_bb["xmax"] + .05*(pr_bb["xmax"] - pr_bb["xmin"]), ymax = pr_bb["ymax"] + .2*(pr_bb["ymax"] - pr_bb["ymin"])), alpha = 0, colour = "black",  inherit.aes = FALSE) +
        theme(
            panel.grid.major = element_line(colour="transparent"),
            legend.position = "none"
        )
    )

    g3 <- final_a +
 annotation_custom(
            grob = alaska,
            xmin = lower_48_bb[1]*1.25,
            ymin = (lower_48_bb[4]*1.75) - calculate_range(alaska_bb)["y_range"],
            xmax = (lower_48_bb[1]*1.25) + calculate_range(alaska_bb)["x_range"],
            ymax =  lower_48_bb[4]*1.75
        ) +
        annotation_custom(
            grob = hawaii,
            xmin = lower_48_bb[1]*1.20,
            ymin = (lower_48_bb[2]*1.5),
            xmax = (lower_48_bb[1]*1.20) + calculate_range(hawaii_bb)["x_range"],
            ymax =  lower_48_bb[2]*1.5 + calculate_range(hawaii_bb)["y_range"]
        ) +
        annotation_custom(
            grob = pr,
            xmin = lower_48_bb[3]*1.25 - calculate_range(pr_bb)["x_range"],
            ymin = (lower_48_bb[2]*1.5),
            xmax = (lower_48_bb[3]*1.25),
            ymax =  lower_48_bb[2]*1.5 + calculate_range(pr_bb)["y_range"]
        ) + annotation_custom(legend, xmin = 2.5*lower_48_bb[3], ymin = .05*lower_48_bb[4], xmax = lower_48_bb[3], ymax = .05*lower_48_bb[4])



ggsave(filename = "combined_0219_test_nosimplify.pdf", plot = g3, width = 40, height = 20, units = "in")

