# cowboy_fcgi

FastCGI handler for [Cowboy](https://github.com/ninenines/cowboy) web server -
allows PHP scripts to run along side your Erlang web applications via `php-fpm`.

This is a fork of [extend/cowboy_fcgi](https://github.com/extend/cowboy_fcgi)
which is no longer maintained in that repo. I have updated the code to support
latest Cowboy 2 stable version.

## status

It is currently tested against stable Cowboy version 2 and PHP 7 (it should
also work with PHP 5).

## getting started

#### requirements

* erlang (19.0 or newer)
* GNU `make` (or rebar3)
* `php-fpm` executable in path

#### quick start

* clone this repo
* `cd cowboy_fcgi/examples/cowboy_php_hello`
* `make run`

This should land you on a console of a running application.

Use a web client or a browser to visit

[http://localhost:33080/hello.php](http://localhost:33080/hello.php)

You should see a simple response from hello.php script. Get more details in the
example's
[README](https://github.com/unix1/cowboy_fcgi/tree/master/examples/cowboy_php_hello).

## docs

Docs coming soon... for now see the examples.

## faq

##### *what is php-fpm?*

[PHP FPM](https://secure.php.net/manual/en/install.fpm.php) is a FastCGI
Process Manager for PHP. It is used by web servers such as Nginx and Apache to
integrate with PHP.
