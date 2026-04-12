{ inputs, purs-nix-instance, ps-pkgs }:

let
  build = purs-nix-instance.build;

  hyrule = build {
    name = "hyrule";
    src.path = inputs.purescript-hyrule;
    info.dependencies = with ps-pkgs; [
      avar effect filterable free js-timers random
      web-html unsafe-reference web-uievents
    ];
  };

  deku-core = build {
    name = "deku-core";
    src.path = inputs.purescript-deku + "/deku-core";
    info.dependencies = with ps-pkgs; [ untagged-union ] ++ [ hyrule ];
  };

  deku-dom = build {
    name = "deku-dom";
    src.path = inputs.purescript-deku + "/deku-dom";
    info.dependencies = with ps-pkgs; [
      web-touchevents web-pointerevents untagged-union
    ] ++ [ hyrule ];
  };

  deku-css = build {
    name = "deku-css";
    src.path = inputs.purescript-deku-css + "/deku-css";
    info.dependencies = with ps-pkgs; [ css ] ++ [ deku-core hyrule ];
  };

  dodo-printer = build {
    name = "dodo-printer";
    src.path = inputs.purescript-dodo-printer;
    info.dependencies = with ps-pkgs; [
      ansi arrays avar console control effect either exceptions
      foldable-traversable integers lists maybe minibench newtype
      node-buffer node-child-process node-fs node-os node-path
      node-process node-streams parallel partial prelude safe-coerce
      strings tuples
    ];
  };

  tidy = build {
    name = "tidy";
    src.path = inputs.purescript-tidy;
    info.dependencies = with ps-pkgs; [
      arrays foldable-traversable lists maybe ordered-collections
      partial prelude language-cst-parser strings tuples
    ] ++ [ dodo-printer ];
  };

  tidy-codegen = build {
    name = "tidy-codegen";
    src.path = inputs.purescript-tidy-codegen;
    info.dependencies = with ps-pkgs; [
      aff ansi arrays avar bifunctors console control effect either
      enums exceptions filterable foldable-traversable free identity
      integers language-cst-parser lazy lists maybe newtype node-buffer
      node-child-process node-fs node-path node-process node-streams
      ordered-collections parallel partial posix-types prelude record
      safe-coerce st strings transformers tuples type-equality unicode
    ] ++ [ dodo-printer tidy ];
  };

in with ps-pkgs; [
  # direct deps from spago.yaml
  affjax
  affjax-web
  node-fs
  now
  web-file
  effect
  prelude
  tuples
  web-html
  yoga-json

  # transitive deps required by the custom builds above
  aff
  aff-promise
  arrays
  console
  css
  debug
  effect
  enums
  filterable
  foldable-traversable
  free
  js-timers
  language-cst-parser
  lists
  maybe
  minibench
  newtype
  node-buffer
  node-child-process
  node-os
  node-path
  node-process
  node-streams
  numbers
  ordered-collections
  parallel
  partial
  random
  safe-coerce
  strings
  transformers
  untagged-union
  unsafe-reference
  uuid
  web-events
  web-pointerevents
  web-touchevents
  web-uievents

  # custom builds
  hyrule
  deku-core
  deku-dom
  deku-css
  dodo-printer
  tidy
  tidy-codegen
]