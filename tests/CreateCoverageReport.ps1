cd Win64\Debug
CodeCoverage.exe -dproj .\..\..\soccertests.dproj -od .\coverage\ -html
start coverage\CodeCoverage_summary.html
cd ..\..