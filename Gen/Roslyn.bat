rem If you don't have the Roslyn CTP installed, you must download and install the libraries
rem to this directory. This script will do that.
set path=..\..\..\..\.nuget
nuget.exe install Froto.Roslyn -version 0.0.0.1 -o ..\..\..\
copy /y ..\..\..\Roslyn.Compilers.Common.1.2.20906.2\lib\net45\Roslyn.Compilers.dll
copy /y ..\..\..\Roslyn.Compilers.CSharp.1.2.20906.2\lib\net45\Roslyn.Compilers.CSharp.dll
copy /y ..\..\..\Roslyn.Services.Common.1.2.20906.2\lib\net45\Roslyn.Services.dll
copy /y ..\..\..\Roslyn.Services.Common.1.2.20906.2\lib\net45\Roslyn.Utilities.dll
copy /y ..\..\..\Roslyn.Services.CSharp.1.2.20906.2\lib\net45\Roslyn.Services.CSharp.dll