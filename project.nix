{ mkDerivation, base, bytestring, conduit, conduit-extra
, containers, directory, HFuse, microlens, mtl, split, stdenv, text
, unix, unordered-containers, zip
}:
mkDerivation {
  pname = "hzipfs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring conduit conduit-extra containers directory HFuse
    microlens mtl split text unix unordered-containers zip
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
