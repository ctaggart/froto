#!/bin/sh -e
SCRIPTROOT="$(cd "$(dirname "$0")"; pwd)"
VERSION="${VERSION:-0.0.0}"
dotnet restore Froto.sln
dotnet build Froto.sln -c Release /p:Version=$VERSION
dotnet test --no-build -c Release Parser.Test/Froto.Parser.Test.fsproj 
dotnet test --no-build -c Release Serialization.Test/Froto.Serialization.Test.fsproj
dotnet test --no-build -c Release TypeProvider.Test/Froto.TypeProvider.Test.fsproj
dotnet pack --no-build -c Release Parser/Froto.Parser.fsproj -o $SCRIPTROOT/bin /p:Version=$VERSION
dotnet pack --no-build -c Release Serialization/Froto.Serialization.fsproj -o $SCRIPTROOT/bin /p:Version=$VERSION
dotnet pack --no-build -c Release TypeProvider.Test/Froto.TypeProvider.Test.fsproj $SCRIPTROOT/bin /p:Version=$VERSION