### kleuren figuren###
cbs_lightblue <- "#00a1cd"
cbs_darkblue <- "#0058b8"
cbs_lightgreen <- "#afcb05"
cbs_darkgreen <- "#53a31d"
cbs_pink <- "#d9328a"
cbs_purple <- "#7d4791"
cbs_orange <- "#f39200"
cbs_red <- "#c90c0f"
cbs_lightblue2 <- "#0581a2"
cbs_darkblue2 <- "#163a72"
cbs_lightgreen2 <- "#899d0c"
cbs_darkgreen2 <- "#488225"
cbs_pink2 <- "#af0e80"
cbs_purple2 <- "#56217a"
cbs_orange2 <- "#da5914"
cbs_red2 <- "#9c1006"


cbsKleuren <- c(cbs_lightblue, cbs_darkblue, cbs_lightgreen, cbs_darkgreen, cbs_pink, cbs_purple, cbs_orange, cbs_red
                 , cbs_lightblue2, cbs_darkblue2, cbs_lightgreen2, cbs_darkgreen2, cbs_pink2, cbs_purple2, cbs_orange2, cbs_red2)
cbsKleuren_naam <- c("cbs_lightblue", "cbs_darkblue", "cbs_lightgreen", "cbs_darkgreen", "cbs_pink", "cbs_purple", "cbs_orange", "cbs_red"
                , "cbs_lightblue2", "cbs_darkblue2", "cbs_lightgreen2", "cbs_darkgreen2", "cbs_pink2", "cbs_purple2", "cbs_orange2", "cbs_red2")
### eind kleuren figuren###



### kleuren kaarten ###
#blue
cbs_map_blue_0 <- "#e1f4fd"
cbs_map_blue_1 <- "#c0e7ff"
cbs_map_blue_2 <- "#77cbe5"
cbs_map_blue_3 <- "#3d95d4"
cbs_map_blue_4 <- "#2256a0"
cbs_map_blue_5 <- "#143564"
cbs_map_blue_6 <- "#09183c"

cbsKaartBlauw_7 <- c(cbs_map_blue_0, cbs_map_blue_1, cbs_map_blue_2, cbs_map_blue_3, cbs_map_blue_4, cbs_map_blue_5, cbs_map_blue_6)
cbsKaartBlauw_7_naam <- c("cbs_map_blue_0", "cbs_map_blue_1", "cbs_map_blue_2", "cbs_map_blue_3", "cbs_map_blue_4", "cbs_map_blue_5", "cbs_map_blue_6")
cbsKaartBlauw_5 <- cbsKaartBlauw_7[2:6]
cbsKaartBlauw_5_naam <- cbsKaartBlauw_7_naam[2:6]

#green
cbs_map_green_0 <- "#f1f6de"
cbs_map_green_1 <- "#edf0c7"
cbs_map_green_2 <- "#c9de85"
cbs_map_green_3 <- "#85bc22"
cbs_map_green_4 <- "#348a3a"
cbs_map_green_5 <- "#0f5f34"
cbs_map_green_6 <- "#114625"

cbsKaartGroen_7 <- c(cbs_map_green_0, cbs_map_green_1, cbs_map_green_2, cbs_map_green_3, cbs_map_green_4, cbs_map_green_5, cbs_map_green_6)
cbsKaartGroen_7_naam <- c("cbs_map_green_0", "cbs_map_green_1", "cbs_map_green_2", "cbs_map_green_3", "cbs_map_green_4", "cbs_map_green_5", "cbs_map_green_6")
cbsKaartGroen_5 <- cbsKaartGroen_7[2:6]
cbsKaartGroen_5_naam <- cbsKaartGroen_7_naam[2:6]



#red
cbs_map_red_0 <- "#fedfc7"
cbs_map_red_1 <- "#ffc597"
cbs_map_red_2 <- "#f89e6b"
cbs_map_red_3 <- "#e74d15"
cbs_map_red_4 <- "#c01f26"
cbs_map_red_5 <- "#82001e"
cbs_map_red_6 <- "#5b0708"

cbsKaartRood_7 <- c(cbs_map_red_0, cbs_map_red_1, cbs_map_red_2, cbs_map_red_3, cbs_map_red_4, cbs_map_red_5, cbs_map_red_6)
cbsKaartRood_7_naam <- c("cbs_map_red_0", "cbs_map_red_1", "cbs_map_red_2", "cbs_map_red_3", "cbs_map_red_4", "cbs_map_red_5", "cbs_map_red_6")
cbsKaartRood_5 <- cbsKaartRood_7[2:6]
cbsKaartRood_5_naam <- cbsKaartRood_7_naam[2:6]

#purple
cbs_map_purple_0 <- "#fbe2ed"
cbs_map_purple_1 <- "#f8c1d9"
cbs_map_purple_2 <- "#e38cbf"
cbs_map_purple_3 <- "#be3e8d"
cbs_map_purple_4 <- "#8b176f"
cbs_map_purple_5 <- "#490045"
cbs_map_purple_6 <- "#2d002c"

cbsKaartPaars_7 <- c(cbs_map_purple_0, cbs_map_purple_1, cbs_map_purple_2, cbs_map_purple_3, cbs_map_purple_4, cbs_map_purple_5, cbs_map_purple_6)
cbsKaartPaars_7_naam <- c("cbs_map_purple_0", "cbs_map_purple_1", "cbs_map_purple_2", "cbs_map_purple_3", "cbs_map_purple_4", "cbs_map_purple_5", "cbs_map_purple_6")
cbsKaartPaars_5 <- cbsKaartPaars_7[2:6]
cbsKaartPaars_5_naam <- cbsKaartPaars_7_naam[2:6]

#grey
cbsKaartGrijs <- "#e5e5e5"
cbsKaartGrijs_naam <- "cbsKaartGrijs"

#gemengde kleuren
cbsKaartPaarsGroen <- c(cbsKaartPaars_5[c(5,3,1)], cbsKaartGrijs, cbsKaartGroen_5[c(1,3,5)])
cbsKaartPaarsGroen_naam <- c(cbsKaartPaars_5_naam[c(5,3,1)], cbsKaartGrijs_naam, cbsKaartGroen_5_naam[c(1,3,5)])
cbsKaartGroenPaars <- rev(cbsKaartPaarsGroen)
cbsKaartGroenPaars_naam <- rev(cbsKaartPaarsGroen_naam)
cbsKaartRoodBlauw <- c(cbsKaartRood_5[c(5,3,1)], cbsKaartGrijs, cbsKaartBlauw_5[c(1,3,5)])
cbsKaartRoodBlauw_naam <- c(cbsKaartRood_5_naam[c(5,3,1)], cbsKaartGrijs_naam, cbsKaartBlauw_5_naam[c(1,3,5)])
cbsKaartBlauwRood <- rev(cbsKaartRoodBlauw)
cbsKaartBlauwRood_naam <- rev(cbsKaartRoodBlauw_naam)

### einde kleuren kaarten ###
