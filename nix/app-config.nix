{ name, ... }:
{
  projectName = name;
  
  vite = {
    viteport = 5173;
    settings = {
      open = true;
      clearScreen = false;
    };
  };
  
  purescript = {
    spagoFile = "./spago.yaml";
    codeDirs = [
      "./src"
      "./test"
    ];
    tests = "./test";
    settings = {
      sourceMaps = true;
    };
  };
}