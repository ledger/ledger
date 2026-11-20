# Non-flake entry point (`nix-build`): flake.nix evaluated through
# flake-compat, at the revision and hash flake.lock pins for it.
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  compat = lock.nodes.${lock.nodes.root.inputs.flake-compat}.locked;
in
(import (fetchTarball {
  url = "https://github.com/edolstra/flake-compat/archive/${compat.rev}.tar.gz";
  sha256 = compat.narHash;
}) { src = ./.; }).defaultNix
