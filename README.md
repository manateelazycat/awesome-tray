<img src="./screenshots/screenshot.png">

[More screenshots](./screenshots/README.md)

### What's this?
I don't like the mode-line, it's too high and affect me to read the code.
With Emacs, we only need to focus on very little information, such as time, current mode, git branch.
Excessive information can seriously interfere with our attention.

## Installation
Clone this repository

```console
$ git clone --depth=1 https://github.com/manateelazycat/awesome-tray.git
```

Then put awesome-tray.el to your load-path.

The load-path is usually `~/elisp/`. It's set in your `~/.emacs` like this:

```Elisp
(add-to-list `load-path (expand-file-name "~/elisp"))
(require 'awesome-tray)
(awesome-tray-mode 1)
```

## Customize Mode line.

- `awesome-tray-hide-mode-line`: Enabled by default, makes the mode-line very thin and highlight it when its active/inactive.
- `awesome-tray-mode-line-active-color`: Use for customize active color.
- `awesome-tray-mode-line-inactive-color`: Use for customize inactive color.
- `awesome-tray-adjust-mode-line-color-enable`: Disabled by default. If non-nil, adjust mode-line  color when buffer state changes.
- `awesome-tray-mode-line-modified-readonly-color`: Use for customize modified and readonly color.
- `awesome-tray-mode-line-readonly-color`: Use for customize readonly color.
- `awesome-tray-mode-line-modified-color`: Use for customize modified color.
- `awesome-tray-mode-line-height`: Mode line height, default is 0.1
- `awesome-tray-date-format`: Use to customize the date string format.
- `awesome-tray-mpd-format`: Use to customize the mpd string format, see the variable docstring for details.
- `awesome-tray-git-format`: Use to customize the git string format.
- `awesome-tray-location-format`: Use to customize the location string format, see `mode-line-format`.
- `awesome-tray-location-info-all`: Use to customize the location "All", if `mode-line-format` contains `%p`.
- `awesome-tray-location-info-top`: Use to customize the location "Top", if `mode-line-format` contains `%p`.
- `awesome-tray-location-info-bottom`: Use to customize the location "Bottom", if `mode-line-format` contains `%p`.
- `awesome-tray-git-show-status`: If non-nil, show current file status on the git module.
- `awesome-tray-ellipsis`: Use to customize the ellipses used when truncating.
- `awesome-tray-separator`: Use to customize the separator between modules.
- `awesome-tray-evil-show-mode`: If non-nil, show current evil mode in the evil module.
- `awesome-tray-evil-show-macro`: If non-nil, show recording macro in the evil module.
- `awesome-tray-evil-show-cursor-count`: If non-nil, show multiple cursors count in the evil module.
- `awesome-tray-github-update-duration`: Update duration of the github notification, in seconds.
- `awesome-tray-github-erase-duration`: Github notification time before it gets removed from the bar, in seconds.
- `awesome-tray-meow-show-mode`: If non-nil, show current meow mode in the meow module.
- `awesome-tray-input-method-default-style`: Input method indicator you want to show when no input method is toggled on.
- `awesome-tray-input-method-local-style`: Input method indicator for your local input method.
- `awesome-tray-input-method-local-methods`: List of input methods as your local input method. If input method is toggled on, but not a member of this list, `input-method-title` will display in as input method indicator in awesome-tray, such as "DE@" for German. Default is "rime".

## Dangerous options
Please read the docstring for those variables

**Those options can make your awesome-tray look weird, if your minibuffer looks weird disable them**

- `awesome-tray-second-line`: [screenshot](./screenshots/screenshot2.png), Displays awesome-tray in a second line keeping the minibuffer messages readable.
- `awesome-tray-position`: [screenshot](./screenshots/centered.png), Displays awesome-tray in the left, right or center, better to be used with `awesome-tray-second-line` enabled.

## Customize Module
You can control modules through option ```awesome-tray-active-modules```.

**When changing the modules load awesome-tray-mode after setting the modules to prevent useless hooks and changes**

You can find all modules name in the keys of variable ```awesome-tray-module-alist```. Currently we have:

- `awesome-tab`: Show group information of [awesome-tab](https://github.com/manateelazycat/awesome-tab).
- `buffer-name`: Show buffer name.
- `circe`: Show circe tracking buffer information.
- `date`: Show current date.
- `celestial`: If you are not settled for date, you can add lunar phase and sunrise/set time. Requires `celestial-mode-line` package.
- `evil`: Show evil state, recording macro and multiple cursors count in both [evil-mc](https://github.com/gabesoft/evil-mc) and [multiple-cursors](https://github.com/magnars/multiple-cursors.el).
- `file-path`: Show file path with full customizability. When the path is long, it can be shrinked into something like `.../.em/el/awesome-tray/awesome-tray.el`. See `awesome-tray-file-path-***` variables for details.
- `git`: Show git information.
- `last-command`: Show last execute command.
- `location`: Show point position in buffer.
- `pdf-view-page`: Show page number in pdf-view-mode.
- `location-or-page`: Show location or pdf page number depends on current mode.
- `parent-dir`: Show direct parent directory.
- `mode-name`: Show major mode name.
- `rvm`: Show Ruby version information given by `rvm-prompt`.
- `battery`: Show battery status.
- `input-method`: Show input method status.
- `buffer-read-only`: Show read only status.
- `belong`: Show which class/function status, need install `treesit` first.
- `org-pomodoro`: Show `org-pomodoro` status. Denote the rest time of pomodoro by `[.]`, short break by `(.)` and long break by `{.}`.
- `flymake`: Show Flymake state.
- `meow`: Show meow state.
- `mpd`: Show mpd information using [libmpdel](https://github.com/mpdel/libmpdel), you need to connect to a mpd profile, use `(libmpdel-connect-profile (libmpdel--select-profile))` unless you have multiple profiles.
- `volume`: Show current volume using [volume.el](https://github.com/dbrock/volume.el).
- `word-count`: Show file and selected region word-count.
- `anzu`: Show searched word count and current index using [anzu](https://github.com/emacsorphanage/anzu).
- `github`: Show github notifications using [async](https://github.com/jwiegley/emacs-async) and [ghub](https://github.com/magit/ghub).
- `hostname`: Show remote buffers hostname.

## Create a Module
Let's create a module that says hello to you. With a module you need:

- A name. Let's simply call it "hello".

- A info function that returns the string to be displayed. Here's a simple one

  ``` emacs-lisp
  (defun my-module-hello-info ()
    (concat "Hello " (user-login-name) "!"))
  ```

  A complex info function may encounter an error, awesome-tray will handle this and not show any information there.

- a face. Let's use a simple yet elegant italic style:

  ``` emacs-lisp
  (defface my-module-hello-face
    '((t (:italic t)))
    "Hello module face."
    :group 'awesome-tray)
  ```

- Awesome-tray uses `awesome-tray-module-alist` to find informations about a module. Let's put ours in it:

  ``` emacs-lisp
  (add-to-list 'awesome-tray-module-alist
             '("hello" . (my-module-hello-info my-module-hello-face)))
  ```

- Now put `"hello"` in the `awesome-tray-active-modules` list, and you will see awesome-tray say hello to you!

If you created a module that could be useful to others, please consider contributing it to awesome-tray!
