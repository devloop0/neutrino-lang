SP=$(HOME)/spectre/opt_build/spectre
AS=/opt/arm_cross_compiler/bin/arm-linux-gnueabihf-as
LD=ld

BUILD_DIR=build
ASM_DIR=$(BUILD_DIR)/asm
O_DIR=$(BUILD_DIR)/o
OUTPUT=$(BUILD_DIR)/neutrinoc

SRC_FILES=$(shell find . -type f -wholename './*.sp' -printf '%P\n')
HDR_FILES=$(shell find . -type f -wholename './*.hsp' -printf '%P\n')
O_FILES=$(patsubst %.sp, $(O_DIR)/%.o, $(subst /,!,$(SRC_FILES)))
ASM_FILES=$(patsubst %.sp, $(ASM_DIR)/%.s, $(subst /,!,$(SRC_FILES)))

all: setup $(OUTPUT)

setup:
	mkdir -p $(ASM_DIR) $(O_DIR)

.SECONDEXPANSION:
$(O_FILES): $(O_DIR)/%.o: $(ASM_DIR)/%.s
	$(AS) -mfloat-abi=hard -mfpu=vfp -c $^ -o $@

.SECONDEXPANSION:
$(ASM_FILES): $(ASM_DIR)/%.s: ./$$(subst !,/,%.sp) $(HDR_FILES)
	$(SP) $<
	mv $(patsubst %.sp, %.s, $<) $@

.SECONDEXPANSION:
compile: setup $(O_FILES)

.SECONDEXPANSION:
link: $(O_FILES)
	$(LD) -o $(OUTPUT) $(O_FILES) -L/usr/include/libspectre -l:libspectre.a

$(OUTPUT): $(O_FILES) link

clean:
	rm -rf $(BUILD_DIR)/

