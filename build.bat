rem Build R library and version
R --vanilla < run-roxygen.R

rem Build library
R CMD build --force gamInflu
R CMD INSTALL --build gamInflu
R CMD check gamInflu
