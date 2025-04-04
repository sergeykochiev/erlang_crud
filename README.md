# About
This is a CRUD HTTP server written in ERLang which is connected to a SQLite database. It is highly expandable, making it easy to add new entities to work with.
# Features
- RESTful and CRUDful
- "No-hardcode" entity validation on PATCH and POST requests
- Ability to add new entities with relative ease
# Usage
All interation with the app happens through its ```main``` module:
```console
$ erl
1> c(main).
2> main:start(). # compiles all modules and starts odbc and inets. by default listens to localhost:3003
3> main:initdb(). # executes sql from schema/schema.sql
4> main:stop(). # stoppes odbc and inets applications
5> main:restart(). # used in development for quick reload (stop and start)
```
# Endpoints
- ```GET /``` (always returns schema)
- ```GET /name```
- ```GET /name/id```
- ```POST /name```
- ```PATCH /name/id```
- ```DELETE /name/id```
# Default schema
The JSON representation of the entities' schema available by default is located in ```schema/schema.json``` in the form of ```[{"{name}":{"{field_name}":"{field_type(arbitrary)}"}]```. It is also the result of a ```/``` query.
# Adding new entities
Before modifying the code, add new ```CREATE TABLE``` directive to ```schema/schema.sql``` to reinit the database using ```main.initdb().``` (or do it manually if you don't want to wipe the db; you can even write a migration, I don't f---ing care). Endpoint path and table names will always be the same (because this is how the app works), so try and keep table and module names the same for the sake of clarity.
After making changes to the database, add your new module name (let's call it ```planet``` for convenience) to inets config inside the ```module/main.erl``` between ```root``` and ```notfound```:
```erlang
inets:start(httpd,
  ...,
  {modules, [root, person, car, engine, planet, notfound]}]).
```
Next, create a new file ```module/planets.erl``` with the following contents:
```erlang
-module(planet).
-export([do/1]).

do(M) ->
    util:default_crud(M, "planet").
```
Lastly, restart the app: call ```main:restart().``` in ERLang shell. You now should be able to interact with the database using CRUD operations at the ```/planet``` endpoint.
# Plans for the near future
- Test coverage (unit and end-to-end)
- PUT method
- "Jsonify" all responses
- Fix strings in JSON response are bytearrays  
# Plans for the future future
- Lower the amount of modifications needed to add a new entity
- Refactor, prettify and optimize (as much as possible)
