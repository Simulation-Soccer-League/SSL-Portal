# SSL Index

Run the following commands in R console in this workspace.

## R Studio

To make sure R Studio acknowledges the version of Node installed on your MacOS system, you may need to start it with `open -na Rstudio`

## Install Dependencies

The portal is using R version 4.4.3 due to limitations on the Ubuntu server.

To install all the dependencies:
```
renv::restore()
```

## MySQL database

The portal uses a MySQL database which needs to be installed and run locally for testing purposes. Dumps with relevant information can be provided upon request.

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
