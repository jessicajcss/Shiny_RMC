# https://www.appsilon.com/post/how-to-share-r-shiny-apps
library(shiny)

# https://gist.github.com/jessicajcss/41f1670fb03e5af65e56429638166bcb

runGist("41f1670fb03e5af65e56429638166bcb")

### GITHUB

# https://github.com/RamiKrispin/shinylive-r
install.packages(c("shinylive", "httpuv"))



#saving shinyapp as app.R into myapp folder to generate an HTML shiny app 'inside' Github.
shinylive::export(appdir = "myapp", destdir = "docs")

# checking
httpuv::runStaticServer("docs/", port=8008)


### SHINYAPP.IO ---- https://www.shinyapps.io/admin/#/dashboard
install.packages('rsconnect')

rsconnect::setAccountInfo(name='rmcqualidadedoar',
                          token='52D20482668963755954F50DC9D8FBFA',
                          secret='<SECRET>')
library(rsconnect)
rsconnect::deployApp('./dados')

### Github error 403 on Shinyapps #https://github.com/satijalab/seurat-wrappers/issues/163
usethis::create_github_token()
usethis::edit_r_environ()
remotes::install_github("jessicajcss/Shiny_RMC")

### BLOGDOWN

# https://liuyanguu.github.io/post/2019/02/24/shiny-in-blogdown/

#########
### For future refs

## Connect Posit
#https://docs.posit.co/connect/how-to/connect-posit-account/
#https://docs.posit.co/connect/user/git-backed/

