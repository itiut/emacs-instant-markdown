instant-markdown.el
====

`instant-markdown.el` is a [instant-markdown-d](https://github.com/suan/instant-markdown-d ) client for Emacs.

This is a fork from [syohex/instant-markdown.el](https://gist.github.com/syohex/4351563 ).

Requirements
----

* [instant-markdown-d](https://github.com/suan/instant-markdown-d )


Installation
----

You can install `instant-markdown.el` from [MELPA](http://melpa.milkbox.net/) with `package.el`

```
 M-x package-install instant-markdown.el
```

Usage
----

#### `instant-markdown-mode`

Run `M-x instant-markdown-mode` to toggle previewing the current buffer by `instant-markdown-d`.

Customization
----

#### `instant-markdown:executable`

The path of the `instant-markdown-d` executable.

#### `instant-markdown:port`

Port number of `instant-markdown-d`.

#### `instant-markdown:turn-on-auto-refresh-delay` (Default is `1`)

The number of seconds of delay time from enabling `instant-markdown-mode` to turning on auto refresh.

This delay time is used to wait for `instant-markdown-d` to be ready.

#### `instant-markdown:idle-delay` (Default is `0.25`)

The number of seconds of idle delay time before auto refreshing.

#### `instant-markdown-mode-lighter`

Lighter of `instant-markdown-mode`. Default is `i-md`.
