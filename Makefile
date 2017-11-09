PROJECT = cowboy_fcgi
PROJECT_DESCRIPTION = FastCGI handler for Cowboy

DEPS = ex_fcgi cowboy
dep_ex_fcgi = git https://github.com/unix1/ex_fcgi 1.0.2
dep_cowboy = git https://github.com/ninenines/cowboy 2.1.0

#TEST_DEPS = inets

include erlang.mk
