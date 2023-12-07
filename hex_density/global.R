# remotes::install_github("qfes/rdeck")
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}

# issues with using older version of R 3.6.2 on rstudio.whalesafe.com, 
#   so needing to install packages from sources:
# usethis::browse_github_pat(); usethis::edit_r_environ() GITHUB_PAT="..."
# remotes::install_github("r-lib/rlang", ref="main")
# remotes::install_github("qfes/tidyassert", ref="main")
# remotes::install_github("qfes/rdeck")
# remotes::install_github("r-lib/cachem", ref="main")
# remotes::install_github("r-lib/fastmap", ref="main")
# remotes::install_github("rstudio/htmltools", ref="main")
# remotes::install_github("rstudio/sass", ref="main")
# remotes::install_github("rstudio/jquerylib", ref="main")
# remotes::install_github("rstudio/bslib", ref="main")
# remotes::install_github("r-lib/httr2", ref="main")
# remotes::install_github("r-lib/tidyselect", ref="main")
# remotes::install_github("r-lib/cli", ref="main")
# remotes::install_github("r-lib/ellipsis", ref="main")

librarian::shelf(
  bslib, dplyr, glue, httr2, qfes/rdeck, sf, shinyWidgets, tidyselect, viridis)

dir_private <- switch(
  Sys.info()[["sysname"]],
  "Darwin" = "/Users/bbest/My Drive/private",
  "Linux"  = "/share/private")

mb_token_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
stopifnot(file.exists(mb_token_txt))

mb_token <- readLines(mb_token_txt)
options(rdeck.mapbox_access_token = mb_token)
# rdeck::mapbox_access_token()