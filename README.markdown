```
      ___  __          ___   ___  ____
     / __)(  )   ___  / __) / __)(  _ \
    ( (__ / (_/\(___)( (_ \( (_ \ ) __/
     \___)\____/      \___/ \___/(__)
```

`cl-ggp` is a tiny framework for writing [general game players][GGP] in Common
Lisp.

The `cl-ggp` system handles the GGP protocol for you and *nothing else*.  If you
plan on doing your own GDL reasoning, this is all you need.

The `cl-ggp.reasoner` system contains a simple Prolog-based reasoner using the
[Temperance][] logic programming library.  It's useful as a starting point for
when writing players.

[GGP]: http://www.ggp.org/
[Temperance]: https://sjl.bitbucket.io/temperance/

* **License:** MIT
* **Documentation:** <https://sjl.bitbucket.io/cl-ggp/>
* **Mercurial:** <https://bitbucket.org/sjl/cl-ggp/>
* **Git:** <https://github.com/sjl/cl-ggp/>
