{ name ? "type-novel", ... }:
{
  inherit name;

  network = {
    host        = "localhost";
    bindAddress = "0.0.0.0";
  };

  vite = {
    port     = 5173;
    settings = { };
  };

  purescript = {
    spagoFile = "./spago.yaml";
    srcDirs   = [ "./src" ];
    testDir   = "./test";
    settings  = { };
  };

  dataDir = "$HOME/.local/share/${name}";
  logDir  = "$HOME/.local/share/${name}/logs";
}