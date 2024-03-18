## code to prepare `ann_sample_example` dataset goes here

ann_sample_example=read.csv("https://raw.githubusercontent.com/TomChupan/data/main/data_example_sample3.csv")

usethis::use_data(ann_sample_example, overwrite = TRUE)
