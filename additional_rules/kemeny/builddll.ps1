if(!(Test-path ".\out"))
{
      New-Item -ItemType Directory -Force -Path ".\out"
}
g++ -Wall -shared .\kemeny.cpp .\kemenyexport.cpp -o .\out\kemenydll.dll "-Wl,--out-implib,out\lib.a" 