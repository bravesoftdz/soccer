cd Win32\Debug
CodeCoverage.exe -dproj .\..\..\soccertests.dproj -od .\coverage\ -html
start coverage\CodeCoverage_summary.html
cd ..\..