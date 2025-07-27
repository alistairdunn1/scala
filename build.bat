@echo off
rem =====================================
rem Build and Check scala R Package (Windows)
rem =====================================

rem Check if R is in PATH
where R >nul 2>nul
if errorlevel 1 (
  echo R is not in your PATH. Please install R and add it to your PATH.
  exit /b 1
)

rem Generate documentation with roxygen2
call R --vanilla < run-roxygen.R
if errorlevel 1 exit /b 1

rem Build the package
call R CMD build --force scala
if errorlevel 1 exit /b 1

rem Install the package
call R CMD INSTALL scala
if errorlevel 1 exit /b 1

rem Check the package
call R CMD check scala
if errorlevel 1 exit /b 1
