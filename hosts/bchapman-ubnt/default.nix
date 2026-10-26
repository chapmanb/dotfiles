{ pkgs, ... }:

{
  imports = [ ./cisco-vpn.nix ./bind-mounts.nix ];

  networking.hostName = "bchapman-ubnt";
  networking.networkmanager.enable = true;
  time.timeZone = "America/New_York";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  system.stateVersion = "24.11";

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.luks.devices.nvme0n1p3_crypt.device =
    "/dev/disk/by-uuid/d03d0617-22d4-4e07-9db5-7fce1563fcc9";
  boot.initrd.services.lvm.enable = true;
  boot.kernelModules = [ "kvm-intel" ];
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  # Keep Ubuntu's /boot/grub and EFI/ubuntu intact, including its fallback entry.
  boot.loader.efi = { canTouchEfiVariables = true; efiSysMountPoint = "/boot/efi"; };
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = false;
    mirroredBoots = [{
      path = "/boot/nixos";
      efiSysMountPoint = "/boot/efi";
      efiBootloaderId = "NixOS";
      devices = [ "nodev" ];
    }];
    extraEntries = ''
      menuentry "Ubuntu (original EFI loader)" {
        search --no-floppy --fs-uuid --set=root F64B-C7D5
        chainloader /EFI/ubuntu/shimx64.efi
      }
    '';
  };
  fileSystems."/" = { device = "/dev/vgubuntu/nixos"; fsType = "ext4"; };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/2b2de632-f678-4b85-9f70-580470793b52";
    fsType = "ext4";
  };
  fileSystems."/boot/efi" = { device = "/dev/disk/by-uuid/F64B-C7D5"; fsType = "vfat"; options = [ "umask=0077" ]; };
  fileSystems."/mnt/ubuntu" = {
    device = "/dev/vgubuntu/root";
    fsType = "ext4";
    options = [ "rw" "nofail" ];
  };
  swapDevices = [{ device = "/dev/vgubuntu/swap_1"; }];

  # Match Ubuntu ownership for shared workspaces and private credentials.
  users.groups.bchapman.gid = 10377;
  users.users.bchapman = {
    isNormalUser = true;
    uid = 10377;
    group = "bchapman";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.xkb = { layout = "us"; options = "ctrl:nocaps"; };
  console.useXkbConfig = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  virtualisation.docker.enable = true;
  environment.systemPackages = with pkgs; [ brave google-chrome wezterm zoom-us ];

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    stdenv.cc.cc zlib bzip2 xz openssl glib libuuid icu libffi
    xorg.libX11 xorg.libXext xorg.libXrender xorg.libXi xorg.libXrandr
    xorg.libxcb libxkbcommon wayland libGL alsa-lib libpulseaudio
  ];
  services.envfs.enable = true;

  security.pki.certificateFiles = [
    ../../certs/GinkgoBioworks_Root_CA.crt
    ../../certs/trust_913614DCD865B47873BD546CD7B31865810DA7B5.crt
    ../../certs/trust_A12886C085A0A8F4D234CED936BDEC1767C3F914.crt
    ../../certs/trust_C5091132E9ADF8AD3E33932AE60A5C8FA939E824.crt
  ];
  security.pki.caCertificateBlacklist = [ "emSign Root CA - G1" "emSign ECC Root CA - G3" ];

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel") && (
        action.id == "org.freedesktop.login1.power-off" ||
        action.id == "org.freedesktop.login1.power-off-multiple-sessions" ||
        action.id == "org.freedesktop.login1.power-off-ignore-inhibit"
      )) {
        return polkit.Result.YES;
      }
    });
  '';
}
