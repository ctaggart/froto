# build, test, pack

# dotnet restore Froto.sln --force-evaluate
# dotnet restore TypeProvider.Test\Froto.TypeProvider.Test.fsproj --force-evaluate

dotnet build -c Release Froto.sln
if ($lastexitcode -ne 0){ exit $lastexitcode }
dotnet build -c Release TypeProvider.Test\Froto.TypeProvider.Test.fsproj
if ($lastexitcode -ne 0){ exit $lastexitcode }

dotnet test --no-build -c Release Parser.Test\Froto.Parser.Test.fsproj
if ($lastexitcode -ne 0){ exit $lastexitcode }
dotnet test --no-build -c Release Serialization.Test\Froto.Serialization.Test.fsproj
if ($lastexitcode -ne 0){ exit $lastexitcode }
dotnet test --no-build -c Release TypeProvider.Test\Froto.TypeProvider.Test.fsproj
if ($lastexitcode -ne 0){ exit $lastexitcode }

dotnet pack --no-build -c Release Parser\Froto.Parser.fsproj -o $psscriptroot/bin
if ($lastexitcode -ne 0){ exit $lastexitcode }
dotnet pack --no-build -c Release Serialization\Froto.Serialization.fsproj -o $psscriptroot/bin
if ($lastexitcode -ne 0){ exit $lastexitcode }
dotnet pack --no-build -c Release TypeProvider.DesignTime\Froto.TypeProvider.DesignTime.fsproj -o $psscriptroot/bin
if ($lastexitcode -ne 0){ exit $lastexitcode }
dotnet pack --no-build -c Release TypeProvider.Runtime\Froto.TypeProvider.Runtime.fsproj -o $psscriptroot/bin
if ($lastexitcode -ne 0){ exit $lastexitcode }