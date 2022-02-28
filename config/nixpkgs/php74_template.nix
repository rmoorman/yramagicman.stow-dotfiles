{pkgs ? import <nixpkgs> {}}:
let myPhp = pkgs.php74.withExtensions ({ enabled, all }: enabled ++ [
    all.xdebug
]); in pkgs.mkShell  {

    name="remote_injury_care";
    buildInputs = with pkgs; [
        myPhp
        php74Packages.composer
        php74Packages.phpcbf
        php74Packages.phpcs
        nodejs
    ];
    shellHook = ''
        php --version;
        composer --version;
    '';
}
