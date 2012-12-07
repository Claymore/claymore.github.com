---
layout: post
title: Git Configuration Tips
date: 2012-11-05 00:07
comments: true
categories: git
---

Probably the first thing you want to do after providing git with your name and email is to color its output. Open `~/.gitconfig` in your favorite editor and add following lines:

```
[color]
	branch = auto
	diff = auto
	interactive = auto
	status = auto
```

If you want to cut down on typing you can also use git aliases. Here is list of mine:

```
[alias]
	st = status -s
	ci = commit
	cim = commit -m
	ciam = commit -a -m
	br = branch
	co = checkout
	df = diff
	lg = log -p
	au = add -u
	history = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
	# git svn related commands
	sup = !git svn fetch && git svn rebase -l
	sci = svn dcommit
```
