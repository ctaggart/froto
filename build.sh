#!/bin/sh -e
SCRIPTROOT="$(cd "$(dirname "$0")"; pwd)"
dotnet restore Froto.sln
dotnet build Froto.sln -c Release
dotnet test --no-build -c Release Parser.Test/Froto.Parser.Test.fsproj 
dotnet test --no-build -c Release Serialization.Test/Froto.Serialization.Test.fsproj
dotnet test --no-build -c Release TypeProvider.Test/Froto.TypeProvider.Test.fsproj
dotnet pack --no-build -c Release Parser/Froto.Parser.fsproj -o $SCRIPTROOT/bin
dotnet pack --no-build -c Release Serialization/Froto.Serialization.fsproj -o $SCRIPTROOT/bin
dotnet pack --no-build -c Release TypeProvider.Test/Froto.TypeProvider.Test.fsproj $SCRIPTROOT/bin
dotnet build Froto.sln -c Release

dotnet test --no-build -c Release Parser.Test/Froto.Parser.Test.fsproj
dotnet test --no-build -c Release Serialization.Test/Froto.Serialization.Test.fsproj

dotnet build -c Release TypeProvider.Test/Froto.TypeProvider.Test.fsproj
dotnet test --no-build -c Release TypeProvider.Test/Froto.TypeProvider.Test.fsproj

dotnet pack --no-build -c Release Parser/Froto.Parser.fsproj -o $SCRIPTROOT/bin
dotnet pack --no-build -c Release Serialization/Froto.Serialization.fsproj -o $SCRIPTROOT/bin
dotnet pack --no-build -c Release TypeProvider.Runtime/Froto.TypeProvider.Runtime.fsproj -o $SCRIPTROOT/bin