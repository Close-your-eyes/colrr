glasbey_help <- function() {

  message("Make sure R uses same python as system. ", "system('which python3')")
  message("reticulate::py_config()")
  message("check if glasbey is installed: ", "system('python3 -m pip show glasbey', intern = T)")
  message("install glasbey: system('pip3 install glasbey')")
  message("Import glasbey: gb <- reticulate::import('glasbey')")
  message("See help, e.g. all arguments: reticulate::py_help(gb)")
  message("Run glasbey funs with $ operator: gb$create_palette(palette_size = 10)")
  message("Return results in prismatic::color()")


}



