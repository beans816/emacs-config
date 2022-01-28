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

> make sure python is installed
> 
> make sure pip is installed (sudo pacman -Syu python-pip)
> 
> make sure ipython is installed using sudo (sudo pip3 install ipython) (https://stackoverflow.com/questions/63983934/ipython-installed-but-not-found helped me fix 'python shell interpreter ipython cannot be found' when I already had it installed)

m-x package-install RET python-mode

m-x package-install RET company

m-x package-install RET company-tryhard

m-x package-install RET elpy

m-x package-install RET ipython

> copy code under python comment

# c/c++ code editor

m-x package-install RET eglot

m-x package-install RET company-c-headers

> copy code under c/c++ comment

# other

m-x package-install RET swiper
m-x package-install RET package+
m-x package-install RET package-filter
m-x package-install RET vertigo
