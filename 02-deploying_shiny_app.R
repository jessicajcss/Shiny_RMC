# https://www.appsilon.com/post/how-to-share-r-shiny-apps

# https://gist.github.com/jessicajcss/41f1670fb03e5af65e56429638166bcb

library(shiny)

runGist("41f1670fb03e5af65e56429638166bcb")



# https://github.com/RamiKrispin/shinylive-r
install.packages(c("shinylive", "httpuv"))

#saving shinyapp as app.R into myapp folder to generate an HTML shiny app 'inside' Github.
shinylive::export(appdir = "myapp", destdir = "docs")

# checking
httpuv::runStaticServer("docs/", port=8008)





#########
### For future refs

## Connect Posit
#https://docs.posit.co/connect/how-to/connect-posit-account/
#https://docs.posit.co/connect/user/git-backed/

