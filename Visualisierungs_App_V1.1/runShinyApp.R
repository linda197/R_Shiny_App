Sys.setenv("RSTUDIO_PANDOC"=normalizePath("./Pandoc"))
message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
launch.browser = function(appUrl, browser.path=chrome.portable) {
  message('Browser path: ', browser.path)
  shell(sprintf('"%s" --app=%s', browser.path, appUrl))
  shell(sprinf("Pandoc Pfad = %s",Sys.getenv("RSTUDIO_PANDOC")))
}

shiny::runApp('./shiny/', launch.browser=browseURL)
