# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
 
  boot.kernelPackages = pkgs.linuxPackages_3_16;

  boot.extraModprobeConfig = ''
    options snd_hda_intel enable=0,1
  '';

  time.timeZone = "Canada/Eastern";

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless.
  networking.interfaceMonitor.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  nixpkgs.config = {

    allowUnfree = true;

    firefox = {
     enableGoogleTalkPlugin = true;
     enableAdobeFlash = true;
    };

  };


  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    vimHugeX 
    sudo 
    manpages 
    gitAndTools.gitFull
    iptables
    tcpdump
    rxvt_unicode
    gcc
    binutils
    chromium
    firefoxWrapper
    trayer
    haskellPackages.xmobar
    xfontsel
    xlsfonts
    dejavu_fonts
    dmenu
    xclip
    htop
    irssi
    jq
    gnumake
    dropbox
    lsof
    slock
    evince
    xpdf
    ack
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";

  # key maps
  services.xserver.xkbOptions = "altwin:ctrl_win, altwin:ctrl_alt_win, caps:super";

  # Enable xmonad
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.default = "none";

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xlibs.xset}/bin/xset r rate 200 60
    [[ -f ~/.Xresources ]] && ${pkgs.xlibs.xrdb}/bin/xrdb -merge ~/.Xresources
  '';

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  # mousies
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.twoFingerScroll = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.aterica = {
    name = "aterica";
    group = "users";
    uid = 1001;
    createHome = true;
    password = "changeme";
    home = "/home/aterica";
    extraGroups = ["wheel"];
    shell = "/run/current-system/sw/bin/bash";
  };

  users.extraUsers.aaronlevin = {
    name = "aaronlevin";
    group = "users";
    uid = 1000;
    createHome = true;
    home = "/home/aaronlevin";
    extraGroups = ["wheel"];
    shell = "/run/current-system/sw/bin/bash";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = [
      pkgs.corefonts
      pkgs.clearlyU
      pkgs.cm_unicode
      pkgs.dejavu_fonts
      pkgs.freefont_ttf
      pkgs.terminus_font
      pkgs.ttf_bitstream_vera
    ];
  };

}
