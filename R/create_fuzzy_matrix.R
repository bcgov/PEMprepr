#' Create a fuzzy matrix
#'
#' @param map_unit Single BGC to create fuzzy for
#' @param edatopic Data.table of edatopic data
#' @param neighbours data.table of neighbours
#' @return Matrix of fuzziness
#' @import data.table
#'
#' @export

#' create_fuzzy_matrix("SBSdk",edatopic = eda, neighbours = nbrs)

library(data.table)
edatopic <- fread("D:/PEM_DATA/BEC_DevExchange_Work/_data_allBC/Edatopic_v12_11.csv")
map_unit <- "SBSmc2"


create_fuzzy_matrix <- function(map_unit, edatopic, neighbours){
   #unit <- "SBSmc2"
   #nbrs <- fread("./fuzzy_metrics/edatopic_neighbours.csv")
   #eda <- fread("./fuzzy_metrics/Edatopic_v12_11.csv")

  nbrs <- neighbours
  eda_sub <- edatopic[BGC == map_unit,.(SS_NoSpace,Edatopic)]
  all_ss <- unique(eda_sub$SS_NoSpace)
  eda_same <- merge(eda_sub, eda_sub, by = "Edatopic", all = T, allow.cartesian = T)
  setorder(eda_same, SS_NoSpace.x, SS_NoSpace.y)
  eda_sum <- eda_same[,.(NumSame = .N), by = .(SS_NoSpace.x,SS_NoSpace.y)]

  eda_nb <- copy(nbrs)
  eda_nb[eda_sub, SS_NoSpace := i.SS_NoSpace, on = c(target = "Edatopic")] ##reverse?
  eda_nb[,id_var := 1:nrow(eda_nb)]
  remove_id <- numeric()
  for(bgc in unique(eda_nb$SS_NoSpace)){
    temp <- intersect(eda_nb[SS_NoSpace == bgc, target],
                      eda_nb[SS_NoSpace == bgc, fuzzy])
    remove_id <- c(remove_id,eda_nb[SS_NoSpace == bgc & fuzzy %in% temp,id_var])
  }
  eda_nb <- eda_nb[!id_var %in% remove_id,]

  eda_nb[,c("target","id_var") := NULL]
  eda_nb <- merge(eda_nb, eda_sub, by.x = "fuzzy", by.y = "Edatopic",
                  all = T, allow.cartesian = T)
  eda_nbsum <- eda_nb[,.(NumNb = .N), by = .(SS_NoSpace.x,SS_NoSpace.y)]
  eda_nbsum <- na.omit(eda_nbsum)
  eda_join <- merge(eda_sum, eda_nbsum, by.x = c("SS_NoSpace.x","SS_NoSpace.y"),
                    by.y = c("SS_NoSpace.y","SS_NoSpace.x"), all = T)
  eda_join <- merge(eda_join, eda_nbsum, by.x = c("SS_NoSpace.x","SS_NoSpace.y"),
                    by.y = c("SS_NoSpace.x","SS_NoSpace.y"), all = T)
  eda_join[is.na(NumSame), NumSame := 0]
  eda_join[is.na(NumNb.x), NumNb.x := 0]
  eda_join[is.na(NumNb.y), NumNb.y := 0]

  eda_join[,FMetric := NumSame * 0.05 + NumNb.x * 0.025 + NumNb.y * 0.025]
  eda_join[SS_NoSpace.x == SS_NoSpace.y, FMetric := 1]

  mat <- dcast(eda_join, SS_NoSpace.x ~ SS_NoSpace.y, value.var = "FMetric")
  mat[is.na(mat)] <- 0
  return(mat)
}


