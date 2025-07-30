@echo off
rem Update package version from git commit date
R.exe --vanilla --slave -e "source('update-version.R')"
