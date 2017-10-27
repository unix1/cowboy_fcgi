# Cowboy PHP Hello example

To try this example you'll need

* erlang (19.0 or newer)
* GNU `make`, `git`, and `php-fpm` in your PATH

To build and run this example, type:

    $ make run

Then point your browser to
[http://localhost:33080/hello.php?name=test](http://localhost:33080/hello.php?name=test)

### What happened in the background

* dependencies (`cowboy`, etc.) were installed, release was created and started
* `php-fpm` was started with 5 processes on port `33000`
* `ex_fcgi` fcgi client was started to connect to `php-fpm`
* `cowboy` was started with `cowboy_fcgi` handler on port `33080`

### How to play

###### Add your own PHP scripts

Add your own PHP files in `priv/www` directory and `make run` the project.

###### Change php-fpm configuration

Change `/priv/conf/php-fpm.conf` and `make run` the project.

###### Stopping and restarting

If you don't gracefully stop the application from its shell via
`application:stop(cowboy_php_hello)`, the `php-fpm` processes will stay
running. In this case, just run `killall php-fpm` before doing `make run`
again.

###### Quitting

To gracefully stop the application type

```erlang
application:stop(cowboy_php_hello).
```

in the erlang shell.

You can press `Ctrl-C` twice to quit the shell.
