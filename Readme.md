Todo
============================

This small web application will keep track of your todo list. It's written in Idris and demonstrates how the dependent type system of Idris can be used to improve abstraction and safety in a web server program.

![Screenshot of the application](https://raw.githubusercontent.com/leon-vv/todo/master/screenshot.png)

Usage
-----------------------------
Make sure to install the latest version of the Idris compiler. This package has a dependency on the following packages:

* [record\_](https://github.com/leon-vv/Record) (standalone)
* effects (bundled with Idris)
* [html](https://github.com/leon-vv/Html) (standalone)
* [file](https://github.com/leon-vv/File) (standalone, wrapper around a single function)
* [ferryjs](https://github.com/leon-vv/FerryJS) (dependencies: record\_)
* [event](https://github.com/leon-vv/Event) (dependencies: record\_, ferryjs)
* [http](https://github.com/leon-vv/Http) (dependencies: record\_, ferryjs, event)
* [sql](https://github.com/leon-vv/Sql) (dependencies: record\_, ferryjs, event)

Install these packages manually (see their readme files). Then run:

```
idris -p effects -p record_ -p ferryjs -p html -p sql -p event -p http -p file --codegen node -o todo.js ./Main.idr
```

The produced JavaScript program `todo.js` requires the following external Node.js packages to run:
```
npm install html-entities pg
```

The `todo.js` script accepts the database name, user name and password as command line arguments:

```
node ./todo.js database-name username password
```

The database should contain a table named "todo":

```SQL
CREATE TABLE todo
(
  name character varying NOT NULL,
  done boolean NOT NULL,
  id serial PRIMARY KEY
);
```
License
----------------------------
Mozilla Public License, v. 2.0
