		include ../Makefile.defs

SSD=$(SSDS)/blitbios.ssd
DEPLOY=$(DEPLOY_TOP)/boot80188

AS=nasm
ASFLAGS=
MAP2SYM=$(SCRIPTS)/map2sym.pl

OBJS=blitbios.rom
EXTRAS=$(basename $(wildcard extras/*.inf))

OBJS_O=$(addprefix $(BUILD)/, $(OBJS))

.DELETE_ON_ERROR:

.PHONY: clean all $(SUBDIRS)

all: $(OBJS_O)
ssd: all $(SSD)

clean: $(SUBDIRS)
	-rm $(SSD)
	-rm $(BUILD)/blitbios.rom
	-rm $(BUILD)/blitbios.rom.inf
	-rm $(BUILD)/blitbios.noi
	-rm $(BUILD)/blitbios.map
	-rm $(BUILD)/blitbios.lst

deploy: ssd
	mkdir -p $(DEPLOY)
	dfs read -i -d $(DEPLOY) $(SSD)


$(BUILD)/blitbios.rom: main.asm
	$(AS) $(ASFLAGS) -fbin -o $@ -l $(basename $@).lst $< >$(basename $@).map
	$(MAP2SYM) $(basename $@).map $(basename $@).noi .text=FC00
	echo "M.BIOS FFFFC000 FFFFC000" >$@.inf


$(SSD):	$(OBJS_O)
	dfs form -80 $(SSD)
	dfs opt4 -3 $(SSD)
	dfs title $(SSD) "blitbios"
	dfs add $(SSD) $(addsuffix .inf, $(OBJS_O) $(EXTRAS))