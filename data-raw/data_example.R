## code to prepare `data_example` dataset goes here

data_example=read.csv("https://raw.githubusercontent.com/TomChupan/data/main/data_example3.csv")

usethis::use_data(data_example, overwrite = TRUE)
