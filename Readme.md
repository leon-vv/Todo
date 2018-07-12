Todo
============================

This small web application will keep track of your todo list. It's written in Idris and demonstrates how the dependent type system of Idris can be used to improve abstraction and safety in a web server program.

![Screenshot of the application](https://raw.githubusercontent.com/leon-vv/todo/master/screenshot.png)

Usage
-----------------------------
Make sure to install the latest version of the Idris compiler. This package has a dependency on the following packages:

* [record\_](https://github.com/leon-vv/Record)
* effects (bundled with Idris)
* [http](https://github.com/leon-vv/Http)
* [event](https://github.com/leon-vv/Event)
* [sql](https://github.com/leon-vv/Sql)
* [html](https://github.com/leon-vv/Html)
* [ferryjs](https://github.com/leon-vv/FerryJS)
* [file](https://github.com/leon-vv/File) (wrapper around a single function)

Install these packages manually (see their readme files). Then run:
```idris --build todo.ipkg```

License
----------------------------
Mozilla Public License, v. 2.0
