PROJECT = mysql_pool
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = poolboy mysql
dep_poolboy = git git@github.com:devinus/poolboy.git 1.5.0
dep_mysql = git https://github.com/mysql-otp/mysql-otp.git 1.3.2

LOCAL_DEPS = crypto ssl inets public_key
include erlang.mk

purge:
	rm -rf deps/*/ebin/*.app
	rm -rf deps/*/.idea
	rm -rf deps/*/.erlang.mk
	rm -rf deps/*/test
	rm -rf deps/*/examples
	rm -rf deps/*/doc
	rm -rf deps/*/.git
	rm -rf deps/*/*.d