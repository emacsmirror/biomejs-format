# biomejs-format.el

biomejs-format is a function that formats the current buffer using [Biome](https://github.com/biomejs/biome). The
package also exports a minor mode that applies `(biomejs-format)` on save.

## Configuration

### Requirements

Ensure that the biome program is installed:

```bash
which biome
```

If biome is not installed already, please refer https://biomejs.dev/guides/getting-started/#installation .


### Basic configuration

First require the package:

```elisp
(require 'biomejs-format)
```

Then you can hook to your favorite javascript mode:

```elisp
(add-hook 'js2-mode-hook 'biomejs-format-mode)
(add-hook 'web-mode-hook 'biomejs-format-mode)
...
```

### Biome arguments

To adjust the CLI args used for the biome command, you can customize the `biomejs-format-biome-args` variable:

```elisp
(setq biomejs-format-biome-args '(
  "format"
  "--trailing-comma" "all"
  "--bracket-spacing" "false"
))
```

### Usage with web-mode

Web-mode is a popular mode for editing .js and .jsx files, but it is used to edit other template files too. If you want to hook biomejs-format to web-mode for .js and .jsx files only, you can define a helper function like this:

```elisp
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
```

And then hook to web-mode like this:

```elisp
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . biomejs-format-mode))))
```
## Installing on Windows

This package requires the `diff` tool which is already included on Unix platforms. The simplest way to install `diff` on Windows is to use [Chocolatey](https://chocolatey.org/). The steps are as follows:

1. Follow the Chocolatey install instructions: https://chocolatey.org/install
2. Open an Admin Powershell session
3. Install the `diff` program: `choco install diffutils`

You should now be able to open Emacs and successfully use this package.

## Customization

This package is customizable via custom.el:

```
M-x customize-group biomejs-format
```

* `biomejs-format-biome-command` is the biome command
* `biomejs-format-biome-args` are the args passed to the biome command
* `biomejs-format-show-errors` customizes where to display the error output (buffer, echo or nil)
* `biomejs-format-width-mode` customizes the width when formatting buffer contents (window, fill or nil)

## Using node_modules/.bin/biome

If you want to use your project's biome version you can rely on https://github.com/codesuki/add-node-modules-path

```elisp
(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'biomejs-format-mode)))
```
