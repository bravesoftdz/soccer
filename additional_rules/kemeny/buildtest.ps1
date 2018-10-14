if(!(Test-Path ".\out"))
{
      New-Item -ItemType Directory -Force -Path ".\out"
}
gcc -Wall kemenytest.cpp kemeny.cpp -g3 -lstdc++ -o .\out\kemeny.exe