; derived mode for openbsd pf syntaxhilighting. By a lisp-n00b
(defgroup pf-faces nil
  "Typefaces used for pf-syntax-hilighting"
  :group 'pf
  :group 'faces)

(defface pf-ip-address-face
  '((((class color)
      (background light))
     (:foreground "Navy" :bold t))
    (((class color)
      (background dark))
     (:foreground "LightBlue" :bold t))
    (t
     (:bold t)))
  "Face for ip-addresses"
  :group 'pf-faces)

(defface pf-macro-face
  '((((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (((class color)
      (background dark))
     (:foreground "LightSteelBlue")))
  "Face used for macros"
  :group 'pf-faces)

(defface pf-macro-def-face
  '((((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (((class color)
      (background dark))
     (:foreground "LightSteelBlue")))
  "Face used for defining macros"
  :group 'pf-faces)


(defface pf-from-to-face
  '((((class color)
      (background light))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "green")))
  "Face used for variables"
  :group 'pf-faces)

(defface pf-macro-face
  '((((class color)
      (background light))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "yellow")))
  "Face used for anything within {} and thus a macro."
  :group 'pf-faces)

(defface pf-state-face
  '((((class color)
      (background light))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "magenta")))
  "Face used for a correct state declaration."
  :group 'pf-faces)

(defface pf-proto-spec-face
  '((((class color)
      (background light))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "blue")))
  "Face used for a correct proto-spec declaration."
  :group 'pf-faces)

(defface pf-rule-start-face
  '((((class color)
      (background light))
     (:foreground "pink"))
    (((class color)
      (background dark))
     (:foreground "pink")))
  "Face used for a correct start of a rule."
  :group 'pf-faces)

(defface pf-hostmask-num-face
  '((((class color)
      (background light))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "green")))
  "Face used for a correct numerical hostmask."
  :group 'pf-faces)

(defface pf-address-family-face
  '((((class color)
      (background light))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "green")))
  "Face used for a correct address family. either inet or inet6"
  :group 'pf-faces)

(defface pf-port-spec-face
  '((((class color)
      (background light))
     (:foreground "green" :background "blue"))
    (((class color)
      (background dark))
     (:foreground "green")))
  "Face used for a correct port portnumber/servicename"
  :group 'pf-faces)

(defface pf-flags-spec-face
  '((((class color)
      (background light))
     (:foreground "red" :background "blue"))
    (((class color)
      (background dark))
     (:foreground "red")))
  "Face used for a correct flags-secifier"
  :group 'pf-faces)

(defface pf-single-keyword-face
  '((((class color)
      (background light))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "yellow")))
  "Face used for a single valid keywords"
  :group 'pf-faces)


(define-generic-mode 'pf-mode
  '("#")
  nil
  '(
    ("\\/\\([1-2]\\{0,1\\}[0-9]\\|3[0-2]\\)\\>" . 'pf-hostmask-num-face)
    ("\\(\\(1??[0-9]\\{1,2\\}\\|2[0-4][0-9]\\|25[0-5]\\)\\.\\)\\{3\\}\\(25[0-5]\\|2[0-4][0-9]\\|1??[0-9]\\{1,2\\}\\)" . 'pf-ip-address-face)
    ("\\<\\(from\\|to\\)\\>" . 'pf-from-to-face)
    ("\\<\$[a-zA-Z][a-zA-Z_0-9]+\\>" . 'pf-macro-face)
    ("\\<[a-zA-Z][a-zA-Z_0-9]+\\W=\\W" . 'pf-macro-def-face)
    ("\\<\\(keep\\|modulate\\)\\s-state\\>" . 'pf-state-face)
    ("\\<proto\\s-\\(3pc\\|a/n\\|ah\\|any\\|argus\\|aris\\|ax.25\\|bbn-rcc-mon\\|bna\\|br-sat-mon\\|cftp\\|chaos\\|compaq-peer\\|cphb\\|cpnx\\|crtp\\|crudp\\|dcn-meas\\|ddp\\|ddx\\|dgp\\|egp\\|emcon\\|encap\\|esp\\|etherip\\|fc\\|fire\\|ggp\\|gmtp\\|gre\\|hmp\\|i-nlsp\\|iatp\\|icmp\\|idpr\\|idpr-cmtp\\|idrp\\|ifmp\\|igmp\\|igp\\|igrp\\|il\\|ip\\|ipcomp\\|ipcv\\|ipencap\\|ipip\\|iplt\\|ippc\\|ipv6\\|ipv6-icmp\\|ipv6-nonxt\\|ipv6-opts\\|ipx-in-ip\\|irtp\\|isis\\|iso-ip\\|iso-tp4\\|kryptolan\\|l2tp\\|larp\\|leaf-1\\|leaf-2\\|merit-inp\\|mfe-nsp\\|mhrp\\|micp\\|mobileip\\|mtp\\|mux\\|netblt\\|nhrp\\|nsfnet-igp\\|nvp-ii\\|ospf\\|pgm\\|pim\\|pipe\\|pnni\\|prm\\|ptp\\|pup\\|pvp\\|qnx\\|rdp\\|reserved\\|rsvp\\|rsvp-e2e-ignore\\|rvd\\|sat-expak\\|sat-mon\\|scc-sp\\|scps\\|sctp\\|sdrp\\|secure-vmtp\\|sep\\|sip-frag\\|sip-sr\\|skip\\|sm\\|smp\\|snp\\|sprite-rpc\\|sps\\|srp\\|sscopmce\\|st\\|stp\\|sun-nd\\|swipe\\|tcf\\|tcp\\|tp++\\|trunk-1\\|trunk-2\\|ttp\\|ucl\\|udp\\|uti\\|vines\\|visa\\|vmtp\\|vrrp\\|wb-expak\\|wb-mon\\|wsn\\|xnet\\|xns-idp\\|xtp\\|{.*}\\)\\>" . 'pf-proto-spec-face)
    ("\\<\\(pass\\|block\\|scrub\\)\\s-\\(in\\|out\\)\\>" . 'pf-rule-start-face)
    ("\\<\\(inet\\|inet6\\)\\>" . 'pf-address-family-face)
    ("\\<port \\((at-echo\\|at-nbp\\|at-rtmp\\|at-zis\\|bgp\\|bootpc\\|bootps\\|chargen\\|cmip-agent\\|cmip-man\\|csnet-ns\\|daytime\\|discard\\|domain\\|echo\\|epmap\\|gopher\\|imap\\|imap3\\|imaps\\|ingreslock\\|ipx\\|irc\\|kerberos\\|kerberos-adm\\|kerberos-iv\\|kerberos_master\\|kpasswd\\|krb524\\|ldap\\|ldaps\\|microsoft-ds\\|msp\\|netbios-dgm\\|netbios-ns\\|netbios-ssn\\|nextstep\\|nfsd\\|ntp\\|photuris\\|pop2\\|pop3\\|pop3s\\|pptp\\|prospero\\|prospero-np\\|pwdgen\\|rfe\\|rtelnet\\|rtsp\\|silc\\|sip\\|smux\\|ssdp\\|ssh\\|submission\\|sunrpc\\|svrloc\\|time\\|ulistserv\\|webster\\|www\\|xdmcp\\|z3950\\|afs3-bos\\|afs3-callback\\|afs3-errors\\|afs3-fileserver\\|afs3-kaserver\\|afs3-prserver\\|afs3-rmtsys\\|afs3-update\\|afs3-vlserver\\|afs3-volser\\|auth\\|bftp\\|biff\\|canna\\|cddb\\|conference\\|courier\\|cvspserver\\|datametrics\\|efs\\|eklogin\\|ekshell\\|ekshell2\\|exec\\|finger\\|ftp\\|ftp-data\\|hostnames\\|https\\|hunt\\|icb\\|iprop\\|isakmp\\|iso-tsap\\|kauth\\|kf\\|kip\\|klogin\\|kpop\\|krb_prop\\|krbupdate\\|kshell\\|kx\\|link\\|login\\|mtp\\|mysql\\|nameserver\\|nbp\\|netnews\\|netstat\\|netwall\\|nnsp\\|nntp\\|ntalk\\|postgresql\\|printer\\|qotd\\|radacct\\|radius\\|remotefs\\|rje\\|rkinit\\|rlp\\|route\\|rsync\\|rtmp\\|sa-msg-port\\|sftp\\|shell\\|smtp\\|snmp\\|snmp-trap\\|socks\\|spamd\\|spamd-cfg\\|supdup\\|supfiledbg\\|supfilesrv\\|syslog\\|systat\\|talk\\|tcpmux\\|telnet\\|tempo\\|tftp\\|timed\\|uucp\\|uucp-path\\|who\\|whois\\|x400\\|x400-snd\\|zip\\|[0-9]\\{1,5\\}\\)" . 'pf-port-spec-face)
    ("flags [FSRPAUEW]*/[FSRPAUEW]+" . 'pf-flags-spec-face)
    ("\\(in\\|any\\|nat\\|out\\|rdr\\|set\\|pass\\|binat\\|block\\|queue\\|scrub\\|table||\anchor\\|antispoof\\|proto\\|quick\\|on\\|all\\|log\\|\-\>\\)\\W" . 'pf-single-keyword-face)
    )
  '("pf.conf")
  nil
  "Major mode for editing pf-config files as described in pf.conf(5).")
