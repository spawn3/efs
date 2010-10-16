CFLAGS=-I/usr/local/lib/erlang/lib/erl_interface-3.6.4/include -I/usr/local/lib/erlang/erts-5.7.4/include
LDFLAGS=-L/usr/local/lib/erlang/lib/erl_interface-3.6.4/lib

APPS=cserver cserver2 cclient ext_ei ext_port example_drv.so

all: $(APPS)

cserver: complex.o cnode_s.o
	gcc $(LDFLAGS) $^ -lerl_interface -lei -lpthread -o $@

cserver2: complex.o cnode_s2.o
	gcc $(LDFLAGS) $^ -lerl_interface -lei -lpthread -o $@

cclient: complex.o cnode_c.o
	gcc $(LDFLAGS) $^ -lerl_interface -lei -lpthread -o $@

ext_port: complex.o erl_comm.o port.o
	gcc $(LDFLAGS) $^ -lerl_interface -lei -lpthread -o $@

ext_ei: complex.o erl_comm.o ei.o
	gcc $(LDFLAGS) $^ -lerl_interface -lei -lpthread -o $@

example_drv.so: complex.o port_driver.o
	gcc -o $@ -fpic -shared $^

%.o : %.c
	gcc -c $(CFLAGS) $^

clean:
	rm -rf *.beam
	rm -rf *.o
	rm -rf $(APPS)

