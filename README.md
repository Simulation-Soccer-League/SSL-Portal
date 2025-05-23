# SSL Index

Run the following commands in R console in this workspace.

## R Studio

To make sure R Studio acknowledges the version of Node installed on your MacOS system, you may need to start it with `open -na Rstudio`

## Install Dependencies

```
install.packages(c("dplyr", "future", "lubridate", "magick", "plotly", "reactable", "rhino", "RMySQL", "shiny", "shiny.router", "shinycssloaders", "shinyFeedback", "shinyjs", "tippy"))
```

## Linting

SASS / CSS: `rhino::lint_sass()`

## Build CSS using SASS
_Requires NodeJS to be installed on your system_

`rhino::build_sass()`

## Lint R

`rhino::lint_r()`

If you see this error you may need to run the app once first.
```
Error in !trace_length(trace) : invalid argument type
```

## Run App

`shiny::runApp()`
