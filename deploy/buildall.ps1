<#
This script builds ready version of soccer
Parameters:
-platform //possible values "win32" "win64"
-buildruntests //builds and runs tests
#>
param(
    [string]$platform = "win64",
    [switch]$buildruntests = $false
)

<#
Important vars
#>
$msbuildpath = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319"

Set-Location ..

#if you want tests
if($buildruntests){

    if($platform -eq "win64"){
        Write-Output "ATTENTION: Tests can be compiled only for win32, changing target to win32"
        $platform = "win32"
    }

    #Compile and run kemeny C++ test program
    Write-Output "---KEMENY C++ TEST PROGRAM---"
    Set-Location .\additional_rules\kemeny
    .\buildtest.ps1
    .\out\kemeny.exe
    Set-Location ..\..

    #Compile and run kemeny Delphi tests
    Set-Location .\additional_rules\kemeny
    .\builddll
    Copy-Item .\out\kemenydll.dll .\tests\Win32\Debug\kemenydll.dll
    Write-Output "---KEMENY DELPHI UNIT TESTS---"
    Set-Location .\tests
    .\externcompile.bat
    Set-Location .\Win32\Debug
    .\kemenydlltestsproject.exe
    Set-Location ..\..\..\..\..\
}

Set-Location .\deploy