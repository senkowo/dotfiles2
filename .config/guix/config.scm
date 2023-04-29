;; gnu guix system config

;; Which modules to import to access the variables
(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)
;(use-package-modules screen ssh)

(operating-system
  (locale "en_US.utf8")
  (timezone "US/New_York")
  (host-name "GNUwU")
  (keyboard-layout
   (keyboard-layout "us,us" "dvorak,"
		    #:options '("grp:alts_toggle" "ctrl:nocaps")))

  ;; user accounts
  (users (cons* (user-account
		  (name "rin")
		  (comment "Rin")
		  (group "users")
		  (home-directory "/home/rin")
		  (supplementary-groups '("wheel"
					  "audio" "video"
					  "netdev")))
		%base-user-accounts))

  ;; globally-installed packages
  ;; (users can also install packages under their own account using:
  ;;  'guix search KEYWORD' and 'guix install PACKAGE')
  ;(packages ((cons screen %base-packages))
  (package (append (list (specification->package "emacs")
			 (specification->package "emacs-exwm")
			 (specification->package "emacs-desktop-environment")
			 (specification->package "nss-certs"))
		   %base-packages))

  ;; system services
  ;; (to search for available services, use:
  ;;  'guix system search KEYWORD')
  (services
   %desktop-services)

  ;; UEFI GRUB with EFI partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(targets '("/boot/efi"))
		;; (targets (list "/dev/sda"))
		(keyboard-layout keyboard-layout)))

  ;; specify mapped device for the encrypted root parition
  ;; (UUID is fetched with 'cryptsetup luksUUID')
  (mapped-devices (list (mapped-device
			  (source (uuid
				   "jfkldsajflkdjsljfklafjaskl"))
			  (target "cryptroot")
			  (type luks-device-mapping))))

  ;; list of file systems that get mounted.
  ;; (UUID can be obtained with 'blkid')
  (file-systems (cons* (file-system
			 (mount-point "/")
			 ;(device (file-system-label "my-root"))
			 (device "/dev/mapper/cryptroot")
			 (type "btrfs")
			 (dependencies mapped-devies))
		       (file-system
			 (mount-point "/boot/efi")
			 (device (uuid
				  "jkgjahjfhdjalkkljflk"
				  'fat32))
			 (type "vfat")))
		%base-file-systems)

  ;; Specify swap file?
  ;; (swap-devices (list (swap-space
  ;;                       (target "/swapfile"))))

    ;; Allow resolution of '.local' host names with mDNS...?
  ;;(name-service-switch %mdns-host-lookup-nss)



  )

;; end of operating system configuration
