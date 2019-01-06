---
title: Why I Like the Fish Shell
date: 2019-01-06T11:00:00
---

I've tried to use [the fish shell](https://fishshell.com) and it was unexpectedly fun! First of all, it's fast! I haven't realized how slow `zsh` can be. Fish doesn't require a complex set up, it works pretty well out of the box. Its [tutorial](https://fishshell.com/docs/current/tutorial.html) is easy to follow and the documentation is compact and to the point. I am also fond of its [design doc](https://fishshell.com/docs/current/design.html). Fish strives to be user-friendly and to have a small set of orthogonal features.

<!--more-->

Fish has an unusual configuration wizard: `fish_config`. It starts a web-server and allows you to choose a theme or set up a prompt with a web-page. Try it out!

Fish config file is located at `~/.config/fish/config.fish`.

You don't have to set up variables in the configuration file, though. Use [universal variables](https://fishshell.com/docs/current/index.html#variables-universal) instead. For example, this command sets and exports universal variable `$EDITOR`:

```sh
set -xU EDITOR /opt/homebrew/bin/vim
```

You can append directories to `$PATH` this way:

```sh
set -U fish_user_paths /opt/homebrew/bin $fish_user_paths
```

This command removes `/opt/homebrew/bin` from `$fish_user_paths`:

```sh
set -U fish_user_paths (string match -v /opt/homebrew/bin $fish_user_paths)
```

Universal variables are saved in `~/.config/fish/fish_variables`. Don't edit the file by hand, use `set -U` instead.

The first thing I've tried to do in fish is to re-create my zsh prompt. It was easier than I thought! You can define your own version of the `fish_prompt` function:

```sh
mkdir -p ~/.config/fish/functions
$EDITOR ~/.config/fish/functions/fish_prompt.fish
```

```sh
# ~/.config/fish/functions/fish_prompt.fish
function fish_prompt
    set -l color_username d488f9
    set -l color_hostname cb4b16
    set -l color_cwd 94fd3a

    printf "%s%s%s at %s%s%s in %s%s%s" \
        (set_color $color_username) $USER (set_color normal) \
        (set_color $color_hostname) $hostname (set_color normal) \
        (set_color $color_cwd) (prompt_pwd) (set_color normal)

    __fish_git_prompt " is working on (%s)"

    printf "\n~> "
end
```

Let's go through the function body:

* `set -l` sets a local variable.
* `set_color` returns a special string that colors everything after it with the given color. `(set_color normal)` resets colors back to default.
* `$USER` holds your username.
* `$hostname` holds your hostname.
* `prompt_cwd` returns your current working directory.
* It took me a while to find `__fish_git_prompt`. It prints various information about your git repository. ` is working on (%s)` will be printed only if you are inside a git repo. `__fish_git_prompt` is documented [here](https://github.com/fish-shell/fish-shell/blob/master/share/functions/__fish_git_prompt.fish).

My version of `fish_prompt` is a multiline prompt and it looks roughly like this:

```sh
claymore at himeji in ~/.config/fish is working on (spacemacs|✚)
~> 
```

Obligatory link to my fish settings: [fish](https://github.com/Claymore/dotfiles/tree/spacemacs/fish).