# Get and unzip the FParsec source from http://www.quanttec.com/fparsec/
# copy this to the FParsec source directory, next to FParsec.sln, then run
set PATH=%SystemRoot%\Microsoft.Net\Framework\v3.5;%PATH%
MsBuild.exe FParsec.sln /p:Configuration=Release
pause