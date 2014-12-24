PROJECT = calcjs

DEPS = cowboy erlydtl merl lager hackney jiffy 
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
#dep_erlydtl = git https://github.com/erlydtl/erlydtl 0.9.4
dep_erlydtl = git https://github.com/erlydtl/erlydtl 0ba11ce75ff8fff860880c6bd07de41e0cc14edc
dep_merl = git https://github.com/erlydtl/merl 28e5b3829168199e8475fa91b997e0c03b90d280
dep_lager = git https://github.com/basho/lager 2.0.3
dep_hackney = git https://github.com/benoitc/hackney 0.9.1
dep_jiffy = git https://github.com/davisp/jiffy 0.13.1

include erlang.mk
