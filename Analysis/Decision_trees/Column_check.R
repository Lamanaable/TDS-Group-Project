f <- sapply(CRC_data_5_year_ordered_V3, is.factor)
CRC_data_5_year_ordered_V3[!f]
names(CRC_data_5_year_ordered_V3[f])
