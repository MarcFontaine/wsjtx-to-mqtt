{ mkDerivation, aeson, aeson-pretty, base, bytestring
, data-default-class, double-conversion, microlens, microlens-th
, mqtt-hs, network, options, stdenv, stm, template-haskell, text
, th-lift-instances, time, wsjtx-udp, yaml
}:
mkDerivation {
  pname = "wsjtx-to-mqtt";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring data-default-class
    double-conversion microlens microlens-th mqtt-hs network options
    stm template-haskell text th-lift-instances time wsjtx-udp yaml
  ];
  homepage = "https://github.com/MarcFontaine/wsjtx-to-mqtt";
  description = "a bridge between WSJTX and the MQTT protocol";
  license = stdenv.lib.licenses.bsd3;
}
