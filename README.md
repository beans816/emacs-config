# emacs-config

emacs config for customisation, python, c and c++

copy paste below:

``(require 'package)
 
(add-to-list 'package-archives
'("melpa" . "http://melpa.org/packages/") t)
 
(package-initialize)
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))``

m-x customize-themes
(pick your preffered default-theme)
