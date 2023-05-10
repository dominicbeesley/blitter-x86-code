		include ../Makefile.defs

SSD=$(SSDS)/blittest.ssd
DEPLOY=$(DEPLOY_TOP)/test80188

AS=nasm
ASFLAGS=
MAP2SYM=$(SCRIPTS)/map2sym.pl

OBJS=blittest.rom
EXTRAS=$(basename $(wildcard extras/*.inf))

OBJS_O=$(addprefix $(BUILD)/, $(OBJS))

.DELETE_ON_ERROR:

.PHONY: clean all $(SUBDIRS)

all: $(OBJS_O)
ssd: all $(SSD)

clean: $(SUBDIRS)
	-rm $(SSD)
	-rm $(BUILD)/blittest.rom
	-rm $(BUILD)/blittest.rom.inf
	-rm $(BUILD)/blittest.noi
	-rm $(BUILD)/blittest.map
	-rm $(BUILD)/blittest.lst

deploy: ssd
	mkdir -p $(DEPLOY)
	dfs read -i -d $(DEPLOY) $(SSD)


$(BUILD)/blittest.rom: main.asm
	$(AS) $(ASFLAGS) -fbin -o $@ -l $(basename $@).lst $< >$(basename $@).map
	$(MAP2SYM) $(basename $@).map $(basename $@).noi .text=FC00
	echo "M.TEST FFFFC000 FFFFC000" >$@.inf


$(SSD):	$(OBJS_O)
	dfs form -80 $(SSD)
	dfs opt4 -3 $(SSD)
	dfs title $(SSD) "blittest"
	dfs add $(SSD) $(addsuffix .inf, $(OBJS_O) $(EXTRAS))