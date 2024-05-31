check_write <- function(vari, f.ids){

  output <- readxl::read_xlsx("inst/extdata/output_list.xlsx") %>%
    dplyr::filter(var.name == vari)

  out.file <- stringr::str_c(f.ids[[output$f.ids.path]][2], "/", output$obj.name)

  if(file.exists(out.file)){
    print(stringr::str_c(vari, " has been previously written to project directory."))
  }


}
