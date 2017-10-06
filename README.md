cowboy_fcgi
===========

FastCGI handler for [Cowboy](https://github.com/ninenines/cowboy) web server -
allows PHP scripts to run along side your Erlang web applications via `php-fpm`.

This is a fork of [extend/cowboy_fcgi](https://github.com/extend/cowboy_fcgi)
which is no longer maintained in that repo. I have updated the code to support
latest Cowboy 1.x stable version.

status
------

It is currently tested against stable Cowboy version 2 and PHP 7 (it should
also work with PHP 5).

what is php-fpm?
----------------

[PHP FPM](https://secure.php.net/manual/en/install.fpm.php) is a FastCGI
Process Manager for PHP. It is used by web servers such as Nginx and Apache to
integrate with PHP.

getting started
---------------

Docs coming soon... for now see the tests.
