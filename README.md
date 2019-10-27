# Home configuration files
This repository is using `home-manager` and `nix`.

## Installation
Clone the repo
```
git clone https://github.com/dnixty/home-config.git ~/home-config
```

Bootstrap home-manager
```
cd ~/home-config
sh ./bootstrap.sh
```

Copy and edit `secrets.nix`.
```
cp ~/home-config/secrets_example.nix ~/home-config/secrets.nix
```

Switch to the new derivation
```
home-manager switch
```
