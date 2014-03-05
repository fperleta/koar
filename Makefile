
.PHONY: all prepare koar clean

all:	prepare koar

include config.mk

NODES_OBJS=cos2pi delays env fwriter lookup noise passive phasor touch wire
KOAR_OBJS=array buf delay main patch patchctl patchvm peers

OBJS=$(addsuffix .o, \
     $(addprefix obj/, $(KOAR_OBJS)) \
     $(addprefix obj/nodes/, $(NODES_OBJS)))

SRCS=$(OBJS:obj/%.o=src/%.c)
DEPS=$(join $(dir $(SRCS)), $(addprefix ., $(notdir $(SRCS:.c=.d))))

include $(DEPS)

prepare:
	@if [ ! -d bin ]; then mkdir bin; fi
	@if [ ! -d obj ]; then mkdir obj; fi
	@if [ ! -d obj/nodes ]; then mkdir obj/nodes; fi

koar:	bin/koar
bin/koar:	$(OBJS)
	$(CC) -o bin/koar $(OBJS) $(CFLAGS) $(LIBS)

clean:
	for i in `find obj/ -name "*.o"`; do rm $$i; done

