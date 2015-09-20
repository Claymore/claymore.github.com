---
title: Go Packages Vendoring with Gitlab
date: 2015-09-20T20:31:00
categories: [ "golang" ]
---

In a software shop you really want to keep all your external dependecies
in your local network to get them faster and more reliably. Copying
external projects into your own repositories is called vendoring. Let's
vendor a github project into a gitlab project. Here is how to do it.

<!--more-->

Let's say you want to use `github.com/foo/bar` in your own package.
Create `third-party` Gitlab group and import `github.com/foo/bar` to
`third-party/bar`. Now you can use it this way:

```go
import "your-gitlab.com/third-party/bar.git.git"
```

What's the deal with the `.git.git` suffix? The second `.git` suffix
tells `go get` that this is a git repository. Bug `go get` strips it so
we have to specify the `.git` suffix twice.
