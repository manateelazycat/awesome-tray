<img src="./screenshot.png">

### What's this?
I don't like mode-line, it's too high, affect me to read the code.
With Emacs, we only need to focus on very little information, such as time, current mode, git branch.
Excessive information can seriously interfere with our attention.

## Installation
Then put awesome-tray.el to your load-path.

The load-path is usually ~/elisp/.

It's set in your ~/.emacs like this:

```Elisp
(require 'awesome-tray)
(awesome-tray-mode 1)
```

## Customize
You can controll modules through option ```awesome-tray-active-modules```
You can find all modules name through variable ```awesome-tray-all-modules```
