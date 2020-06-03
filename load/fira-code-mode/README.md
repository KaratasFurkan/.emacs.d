fira-code-mode
===
### Minor mode for Fira Code ligatures using prettify-symbols.
![screenshot1](https://github.com/jming422/fira-code-mode/raw/master/screenshots/screenshot1.png)

This is a simple minor mode for Fira Code ligatures, built from [these instructions on the FiraCode repo](https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-prettify-symbols).

You don't need to use Fira Code as your main font in Emacs for this to work! Using this minor mode will give you just the ligatures from the Fira Code font; it won't alter your fonts or faces in any other way. (Of course, you're free to use Fira Code as your main font alongside this package - that's what I do!)

## Getting Started

1. Install the Fira Code Symbol font to your system ([Download](https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip)) ([Original post](https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632)).
   - If you don't know how to install a font to your system, check out Fira Code's main repo. They have some [good instructions here](https://github.com/tonsky/FiraCode/wiki/Installing).
   - Thanks to [@siegebell](https://github.com/siegebell) for creating this font.

2. Clone this repo. I keep mine in `~/.emacs.d/git-lisp/fira-code-mode`, but you can put it wherever you like.

3. Enable `fira-code-mode` in your config. Here are some examples:

With [use-package](https://github.com/jwiegley/use-package) (this is the config that I use personally):
```elisp
(use-package fira-code-mode
  :load-path "git-lisp/fira-code-mode"
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes
```
Feel free to remove or change the `:custom` and `:hook` values of course, but those are the ones I've found useful. Most of the ligatures I've disabled are purely preferential; some of them conflicted with other syntax styling for e.g. Clojure reader macros.

Without use-package:
```elisp
(add-to-list 'load-path "~/.emacs.d/git-lisp/fira-code-mode")
(require 'fira-code-mode)
(custom-set-variable 'fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
(add-hook 'prog-mode-hook 'fira-code-mode) ;; Enables fira-code-mode automatically for modes descended from prog-mode
```
Again, the last two lines are optional, but they're a reasonable starting point.

4. Done, enjoy the ligatures!

## Customization
As of now, `fira-code-mode-disabled-ligatures` is the only customizable option. Add a string to this list to prevent that string from being displayed with a ligature.

## Contributing

This is the first Emacs package that I've made, and I'm making it available in hopes that it will make your lives easier as it did mine. I welcome suggestions and contributions, but here are a couple things to be aware of:
- I'm quite new to writing Emacs Lisp, so please be nice! I know I have a lot to learn.
- I don't have a ton of time to spend on this, but I'll do my best to respond to issues and PRs quickly!
- If you've read this far, thank you! I'm very grateful for the chance to make something useful/interesting.
