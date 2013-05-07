
.PHONY: all koar clean

all:	koar

include config.mk

KOAR_OBJS=buf main patch peers

OBJS=$(addsuffix .o, \
     $(addprefix obj/, $(KOAR_OBJS)))

SRCS=$(OBJS:obj/%.o=src/%.c)
DEPS=$(join $(dir $(SRCS)), $(addprefix ., $(notdir $(SRCS:.c=.d))))

include $(DEPS)

koar:	bin/koar
bin/koar:	$(OBJS)
	$(CC) -o bin/koar $(OBJS) $(CFLAGS) $(LIBS)

clean:
	for i in `find obj/ -name "*.o"`; do rm $$i; done

