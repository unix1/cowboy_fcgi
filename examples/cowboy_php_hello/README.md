# Cowboy PHP Hello example

To try this example you'll need

* erlang (>=19.0) installed
* GNU `make`, `git`, and `php-fpm` in your PATH

To build and run this example, type:

    $ make run

Then point your browser to
[http://localhost:33080/hello.php?name=test](http://localhost:33080/hello.php?name=test)

### What happened in the background

* dependencies were installed, release was created and started
* `php-fpm` was started with 5 processes on port `33000`
* `ex_fcgi` fcgi client was started to connect to `php-fpm`
* `cowboy` was started with `cowboy_fcgi` handler on port `33080`
