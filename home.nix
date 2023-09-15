{ config, lib, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "alex";
  home.homeDirectory = "/var/home/alex";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.activation = {
    symlinkDotEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ ! -e $HOME/.emacs.d ]; then
        $DRY_RUN_CMD ln -s $HOME/.config/home-manager/emacs $HOME/.emacs.d
      fi
    '';
  };

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # Text editors
    pkgs.neovim

    # Language Servers
    pkgs.nodePackages.pyright
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".gitconfig".source = ./gitconfig;
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/alex/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.bash = {
    enable = true;
    historyControl = [ "erasedups" ];
    historyIgnore = [ "cd" "ls" ];
    initExtra = builtins.readFile ./bashrc;
    shellAliases = {
      "ls" = "ls -CFhX --color=auto --group-directories-first";
      "pypath" = "echo $PYTHONPATH | tr ':' '\\n'";
    };
    shellOptions = [
      "autocd"       # If the command is not found and matches a directory name, cd into it
      "checkjobs"    # Check for background jobs before exiting
      "checkwinsize" # Automatically update COLUMNS and LINES env vars between each command
      "extglob"      # Enable extended pattern matching features
      "globstar"     # Enable recursive globbing
                     # e.g. './**/*.py' matches every python file in current and all subdirs.
      "histappend"   # On shell exit, append to history file, don't overwrite.
    ];
    sessionVariables = {
      CDPATH = ".:~/Projects";
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      apheleia
      consult
      corfu
      denote
      ef-themes
      embark
      embark-consult
      kind-icon
      magit
      marginalia
      minions
      nix-mode
      orderless
      rustic
      vertico
      yaml-mode
    ];
  };
}
