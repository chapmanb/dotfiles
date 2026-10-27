{ pkgs, ... }:

let
  # Proprietary Cisco Secure Client 5.1 stays outside the world-readable store.
  runtime = pkgs.buildFHSEnv {
    name = "cisco-secureclient-fhs";
    targetPkgs = p: with p; [
      glibc stdenv.cc.cc zlib openssl libxml2 libxslt glib gtk3 atk
      pango cairo gdk-pixbuf dbus nss nspr libsecret libcap libxcrypt-legacy
      xorg.libX11 xorg.libXext xorg.libXi xorg.libXrender xorg.libXtst
      xorg.libXrandr xorg.libxcb mesa alsa-lib systemd coreutils iproute2
    ];
    extraBwrapArgs = [ "--bind /opt/cisco /opt/cisco" ];
    runScript = pkgs.writeShellScript "cisco-exec" ''
      export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
      exec "$@"
    '';
  };
  vpn = pkgs.writeShellScriptBin "vpn" ''
    exec ${runtime}/bin/cisco-secureclient-fhs /opt/cisco/secureclient/bin/vpn "$@"
  '';
  vpnui = pkgs.writeShellScriptBin "vpnui" ''
    exec ${runtime}/bin/cisco-secureclient-fhs /opt/cisco/secureclient/bin/vpnui "$@"
  '';
in {
  boot.kernelModules = [ "tun" ];
  environment.systemPackages = [ vpn vpnui (pkgs.makeDesktopItem {
    name = "cisco-secureclient";
    desktopName = "Cisco Secure Client 5.1";
    exec = "${vpnui}/bin/vpnui";
    terminal = false;
    categories = [ "Network" ];
  }) ];
  services.dbus.enable = true;
  systemd.tmpfiles.rules = [ "d /opt/cisco 0755 root root -" ];

  systemd.services.cisco-secureclient-hydrate = {
    description = "Hydrate official Cisco Secure Client 5.1 from Ubuntu or restored backup";
    unitConfig.RequiresMountsFor = [ "/mnt/ubuntu" ];
    before = [ "vpnagentd.service" ];
    serviceConfig = { Type = "oneshot"; RemainAfterExit = true; UMask = "0077"; };
    path = [ pkgs.coreutils ];
    script = ''
      set -eu
      complete() {
        for binary in vpnagentd vpn vpnui acwebhelper; do
          test -x "$1/bin/$binary" || return 1
        done
        test -s "$1/vpn/profile/ginkgo-corp-profile.xml" &&
          test -s "$1/AnyConnectLocalPolicy.xml"
      }
      if complete /opt/cisco/secureclient; then exit 0; fi
      source=/mnt/ubuntu/opt/cisco/secureclient
      if ! complete "$source"; then
        source=/var/lib/cisco-secureclient-staging/opt/cisco/secureclient
      fi
      if ! complete "$source"; then
        echo "Restore corporate-access-stack into /var/lib/cisco-secureclient-staging first" >&2
        exit 1
      fi
      # Do not overwrite a partial installation without operator review.
      test ! -e /opt/cisco/secureclient
      stage=$(mktemp -d /opt/cisco/.hydrate-XXXXXX)
      trap 'rm -rf "$stage"' EXIT
      cp -a "$source" "$stage/secureclient"
      chown -R root:root "$stage/secureclient"
      mv "$stage/secureclient" /opt/cisco/secureclient
    '';
  };
  systemd.services.vpnagentd = {
    description = "Official Cisco Secure Client 5.1 VPN agent";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" "cisco-secureclient-hydrate.service" ];
    requires = [ "cisco-secureclient-hydrate.service" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${runtime}/bin/cisco-secureclient-fhs /opt/cisco/secureclient/bin/vpnagentd -execv_instance";
      Restart = "on-failure";
    };
  };
}
