# render paper to seperate output folder

output_path <- here::here("analysis","_output")
input_path <- here::here("analysis","paper_draft3.Rmd")
rmarkdown::render(input = input_path, output_dir = output_path)