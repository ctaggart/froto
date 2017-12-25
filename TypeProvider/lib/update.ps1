# updates to the latest TypeProviders SDK code
$src = "https://github.com/fsprojects/FSharp.TypeProviders.SDK/raw/master/src"
Invoke-WebRequest "$src/ProvidedTypes.fsi" -OutFile $psscriptroot\ProvidedTypes.fsi
Invoke-WebRequest "$src/ProvidedTypes.fs" -OutFile $psscriptroot\ProvidedTypes.fs
Invoke-WebRequest "$src/ProvidedTypesTesting.fs" -OutFile $psscriptroot\ProvidedTypesTesting.fs