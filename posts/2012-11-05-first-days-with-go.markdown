---
layout: post
title: First Days with Go
date: 2012-11-05 13:34
comments: true
tags: golang
---

I've seen articles about [Go] [1] programming language appearing on Hacker News
a few times but I didn't pay much attention to them. Yet another language with
nothing special about it or so it looked. It's changed with Rob Pike's [recent talk] [2].

<!-- more -->

I decided to try it out in a simple program that I originally wrote in Python.
The program takes CSV files with [data] [3] about Japanese population fetched
from official sites and produces a template for Russian Wikipedia. The program
also uses an INI-like configuration file. You can see the original source [here] [4].
It's fairly straightforward, the only thing I had to do is to make CSV reader
handle UTF-8 files by introducing `unicode_csv_reader` function.

I wanted the same functionality in a Go program. Reading CSV files seemed the easiest
task to me so I started with it. Go standard library has a package to deal with
such files, it's called `encoding/csv`. Here is an example of how to use it:

```go
file, err := os.Open("example.csv")
if err != nil {
	log.Fatalln("Error:", err)
}
defer file.Close()
reader := csv.NewReader(file)
for {
	record, err := reader.Read()
	if err == io.EOF {
		break
	} else if err != nil {
		log.Fatalln("Error:", err)
	}
	fmt.Println(record)
}
```

We open the file, command Go to close it when noone uses it anymore, create a
`csv.Reader` object then read and print records line by line until we encounter
end of file. Things to note: Go functions can return multiple values; you can defer
execution of a function until the end of the block; Go has basic type inference
(for example, `reader := csv.NewReader(file)` is a shortcut for `var reader
csv.Reader = csv.NewReader(file)`). Go doesn't have exceptions and it's common
idiom to return errors alongside function values. It makes code somewhat more
verbose but it also makes clear when error will be handled.

The next stop was to read an INI-like configuration file. The standard library
doesn't have a package to read them but it was really easy to implement one
myself. I took `encoding/csv` package [source code] [5] and created my own package
based on it. You can look at the result here: [go-config] [6] at GitHub. Later
I found out there are three third-party packages that do the same but all of
them introduce Python's `ConfigParser` API that doesn't suit Go very well, in my
opinion.

It was indeed fun, easy and straightforward as Rob'd claimed in his talk. Go promotes
writing, documenting, testing and sharing packages: it has tools to automatically
reformat your source code (`go fmt`), builtin testing library (`testing` package
and `go test`), document extraction (`go doc`) and even to get, build and install
third-party packages (`go get`) out of the box.

With all pieces in place I'd finished the [program] [7] and it's almost as easy to
read it as Python version.

[1]: http://golang.org "The Go Programming Language"
[2]: http://talks.golang.org/2012/splash.slide#1 "Go at Google"
[3]: https://github.com/Claymore/wiki-japan-stat/tree/golang/data
[4]: https://github.com/Claymore/wiki-japan-stat/blob/master/generator.py
[5]: http://golang.org/src/pkg/encoding/csv/reader.go
[6]: https://github.com/Claymore/go-config
[7]: https://github.com/Claymore/wiki-japan-stat/blob/golang/generator.go
