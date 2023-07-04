		include ../Makefile.defs

SSD_18x=$(SSDS)/bbios18x.ssd
DEPLOY_18x=$(DEPLOY_TOP)/boot80188

SSD_386ex=$(SSDS)/bbios386.ssd
DEPLOY_386ex=$(DEPLOY_TOP)/boot386ex

AS=nasm
ASFLAGS=-I $(INC)
MAP2SYM=$(SCRIPTS)/map2sym.pl

OBJS=bbios18x.rom bbios386ex.rom
EXTRAS=$(basename $(wildcard extras/*.inf))

OBJS_O=$(addprefix $(BUILD)/, $(OBJS))

SECTIONS=.text=FC00 biosdata=40 interrupts=0

.DELETE_ON_ERROR:

.PHONY: clean all $(SUBDIRS)

all: $(OBJS_O)
ssd: all $(SSD_18x) $(SSD_386ex)

clean: $(SUBDIRS)
	-rm $(SSD_18x) $(SSD_386ex)
	-rm $(BUILD)/bbios18x.rom
	-rm $(BUILD)/bbios18x.rom.inf
	-rm $(BUILD)/bbios18x.noi
	-rm $(BUILD)/bbios18x.map
	-rm $(BUILD)/bbios18x.lst
	-rm $(BUILD)/bbios386ex.rom
	-rm $(BUILD)/bbios386ex.rom.inf
	-rm $(BUILD)/bbios386ex.noi
	-rm $(BUILD)/bbios386ex.map
	-rm $(BUILD)/bbios386ex.lst

deploy: ssd
	mkdir -p $(DEPLOY_18x)
	dfs read -i -d $(DEPLOY_18x) $(SSD_18x)

	mkdir -p $(DEPLOY_386ex)
	dfs read -i -d $(DEPLOY_386ex) $(SSD_386ex)




$(BUILD)/bbios18x.rom: main.asm deice.asm video.asm bbc_keyboard.asm $(wildcard $(INC)/*.inc) 
	$(AS) $(ASFLAGS) -DBOARD_18x=1 -fbin -o $@ -l $(basename $@).lst $< >$(basename $@).map
	$(MAP2SYM) $(basename $@).map $(basename $@).noi $(SECTIONS)
	echo "M.BIOS FFFFC000 FFFFC000" >$@.inf

$(BUILD)/bbios386ex.rom: main.asm deice.asm video.asm bbc_keyboard.asm $(wildcard $(INC)/*.inc) 
	$(AS) $(ASFLAGS) -DBOARD_386ex=1-fbin -o $@ -l $(basename $@).lst $< >$(basename $@).map
	$(MAP2SYM) $(basename $@).map $(basename $@).noi $(SECTIONS)
	echo "M.BIOS FFFFC000 FFFFC000" >$@.inf

$(SSD_18x):	$(OBJS_O)
	dfs form -80 $(SSD_18x)
	dfs opt4 -3 $(SSD_18x)
	dfs title $(SSD_18x) "bios18x"
	dfs add $(SSD_18x) $(addsuffix .inf, $(BUILD)/bbios18x.rom $(EXTRAS))

$(SSD_386ex):	$(OBJS_O)
	dfs form -80 $(SSD_386ex)
	dfs opt4 -3 $(SSD_386ex)
	dfs title $(SSD_386ex) "blitbios"
	dfs add $(SSD_386ex) $(addsuffix .inf, $(BUILD)/bbios386ex.rom $(EXTRAS))
