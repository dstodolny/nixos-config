(use-modules (gnu)
						 (gnu system nss)
						 (gnu services xorg)
						 (gnu packages linux)
						 (gnu packages security-token)
						 (gnu services security-token))

(use-service-modules desktop)
(use-package-modules bootloaders certs suckless base idutils)

(define %user-name "dnixty")
(define %hostname "heimdall")

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
											 (startx (xorg-start-command
																#:configuration-file
																(xorg-configuration-file
																 #:extra-config
																 (list %heimdall/xorg-touchpad))))))))

(define %heimdall/sudoers
	(plain-file "sudoers" "\
Defaults umask=0022
Defaults umask_override
root ALL=(ALL) ALL
%wheel ALL=(ALL) NOPASSWD:ALL\n"))

(define %heimdall/hosts
	(plain-file "hostnames" "\
127.0.0.1     localhost heimdall
::1           localhost heimdall
192.168.1.18  asgard
192.168.1.1   niflheim
192.168.1.215 midgard"))

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
                (group "users")

                (supplementary-groups '("wheel" "netdev"
																				"audio" "video"
																				"lp"))
                (home-directory (string-append "/home/" %user-name)))
               %base-user-accounts))

	(sudoers-file %heimdall/sudoers)
	(hosts-file %heimdall/hosts)

  (packages (cons* nss-certs
									 pcsc-lite
									 ccid
									 %base-packages))

	(services (cons*
						 (bluetooth-service)
						 (service pcscd-service-type
											(pcscd-configuration
											 (pcsc-lite pcsc-lite)
											 (usb-drivers (list ccid))))
						 %heimdall/services))

  (name-service-switch %mdns-host-lookup-nss))
