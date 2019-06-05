(use-modules (gnu)
						 (gnu system nss)
						 (gnu services xorg)
						 (gnu services networking)
						 (gnu services cups)
						 (gnu packages cups)
						 (gnu packages linux)
						 (gnu packages bash)
						 (gnu packages emacs-xyz)
						 (gnu packages security-token)
						 (gnu services security-token))

(use-service-modules desktop)
(use-package-modules gnome certs idutils)

(define %user-name "dnixty")
(define %hostname "heimdall")
(define %heimdall-host-aliases "\
192.168.1.18  asgard
192.168.1.1   niflheim
192.168.1.215 midgard\n\n")

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
	(udev-rule
	 "90-backlight.rules"
	 (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
									"RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
									"\n"
									"ACTION==\"add\", SUBSYSTEM==\"backlight\", "
									"RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

;; Mount Nitrokey
(define %nitrokey-udev-rule
	(udev-rule
	 "41-nitrokey.rules"
	 (string-append "ACTION==\"add\", SUBSYSTEM==\"usb\", "
									"ATTR{idVendor}==\"20a0\", ATTR{idProduct}==\"4211\", "
									"ENV{ID_SMARTCARD_READER}=\"1\", ENV{ID_SMARTCARD_READER_DRIVER}=\"gnupg\", GROUP+=\"users\", MODE=\"0666\"")))

(define %heimdall/xorg-touchpad
	"Section \"InputClass\"
        Identifier \"Touchpads\"
        Driver \"libinput\"
        MatchDevicePath \"/dev/input/event*\"
        MatchIsTouchpad \"on\"

        Option \"DisableWhileTyping\" \"on\"
        Option \"MiddleEmulation\" \"on\"
        Option \"ClickMethod\" \"clickfinger\"
        Option \"ScrollMethod\" \"twofinger\"
        Option \"NaturalScrolling\" \"true\"
  EndSection")

(define %heimdall/services
	(modify-services
	 %desktop-services
	 (udev-service-type config =>
											(udev-configuration
											 (inherit config)
											 (rules (append (udev-configuration-rules config)
																			(list %nitrokey-udev-rule
																						%backlight-udev-rule)))))
	 (slim-service-type config =>
											(slim-configuration
											 (inherit config)
											 (auto-login? #f)
											 (xorg-configuration
												(xorg-configuration
												 (extra-config (list %heimdall/xorg-touchpad))))
											 (default-user %user-name)))))

(define %heimdall/sudoers
	(plain-file "sudoers" "\
Defaults umask=0022
Defaults umask_override
root ALL=(ALL) ALL
%wheel ALL=(ALL) NOPASSWD:ALL\n"))

(operating-system
 (host-name %hostname)
 (timezone "Europe/London")
 (locale "en_GB.utf8")

 (bootloader (bootloader-configuration
							(bootloader grub-bootloader)
							(target "/dev/nvme0n1")))

 (kernel linux-libre)
 (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"))

 (mapped-devices
	(list (mapped-device
				 (source "/dev/nvme0n1p3")
         (target "crypthome")
         (type luks-device-mapping))))

 (file-systems (cons* (file-system
											 (device (file-system-label "root"))
											 (mount-point "/")
											 (type "ext4"))
											(file-system
											 (device (file-system-label "boot"))
											 (mount-point "/boot")
											 (type "ext4"))
											(file-system
											 (device "/dev/mapper/crypthome")
											 (mount-point "/home")
											 (type "ext4")
											 (dependencies mapped-devices))
											%base-file-systems))

 (swap-devices '("/dev/nvme0n1p4"))

 (users (cons (user-account
							 (name %user-name)
							 (comment "Dominik Stodolny")
							 (uid 1000)
							 (group "users")

							 (supplementary-groups '("wheel" "netdev"
																			 "audio" "video"
																			 "lp"))
							 (home-directory (string-append "/home/" %user-name)))
							%base-user-accounts))

 (sudoers-file %heimdall/sudoers)
 (hosts-file
	(plain-file "hosts"
							(string-append (local-host-aliases host-name)
														 %heimdall-host-aliases
														 %facebook-host-aliases)))

 (packages (cons* nss-certs
									pcsc-lite
									ccid
									emacs-exwm
									%base-packages))

 (services (cons*
						(bluetooth-service)
						(service pcscd-service-type
										 (pcscd-configuration
											(pcsc-lite pcsc-lite)
											(usb-drivers (list ccid))))
						(service cups-service-type
										 (cups-configuration
											(web-interface? #t)
											(extensions
											 (list cups-filters hplip-minimal))))
						(extra-special-file "/usr/bin/env"
																(file-append coreutils "/bin/env"))
						(extra-special-file "/usr/bin/bash"
																(file-append bash "/bin/bash"))
						%heimdall/services))

 (name-service-switch %mdns-host-lookup-nss))
