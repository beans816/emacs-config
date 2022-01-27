# emacs-config

emacs config for customisation, python, c and c++

copy paste below:
```
(require 'package)
 
(add-to-list 'package-archives
'("melpa" . "http://melpa.org/packages/") t)
 
(package-initialize)
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
```
m-x customize-themes
(pick your preffered default-theme)

# python code editor

m-x package-install RET company

m-x package-install RET company-tryhard

m-x package-install RET elpy

m-x package-install RET ipython

> copy code under python comment

# c/c++ code editor

m-x package-install RET eglot

m-x package-install RET company-c-headers

> copy code under c/c++ comment


