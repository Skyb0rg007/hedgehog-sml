
MLTON := mlton
MLTON_FLAGS :=

SMLNJ := sml
SMLNJ_FLAGS :=

POLY := poly
POLY_FLAGS :=

MLKIT := mlkit
MLKIT_FLAGS := 

DIEHARDER := dieharder

BUILDDIR := _build

##############################################################################

ifeq ($(WORD),32)
MLTON_FLAGS += -default-type word32
endif
ifeq ($(INT),32)
MLTON_FLAGS += -default-type int32
endif
ifeq ($(INT),inf)
MLTON_FLAGS += -default-type intinf
endif
ifeq ($(REAL),32)
MLTON_FLAGS += -default-type real32
endif

SOURCES := $(wildcard hedgehog/*.sml hedgehog/*.sig hedgehog/*.fun)

.DEFAULT: all
.PHONY: all
.PHONY: splitmix-dieharder check-dieharder
.PHONY: smlnj

all:
	@echo "=== Makefile targets ==="
	@echo
	@echo "make typecheck - use MLton to typecheck the library"
	@echo "make clean - remove build artifacts"
	@echo "make splitmix-dieharder - run the dieharder prng benchmark"
	@echo
	@echo "=== Makefile variables ==="
	@echo
	@echo "make WORD=32 - set default word type to Word32.word"
	@echo "make INT=32 - set default int type to Int.int"
	@echo "make INT=inf - set default int type to IntInf.int"
	@echo "make REAL=32 - set default real type to Real32.real"

typecheck: hedgehog/sources.mlb $(SOURCES)
	$(MLTON) $(MLTON_FLAGS) -stop tc hedgehog/sources.mlb

poly: $(BUILDDIR)/hedgehog.poly

smlnj:
	echo | $(SMLNJ) $(SMLNJ_FLAGS) -m hedgehog/sources.cm

mlkit: $(BUILDDIR)/hedgehog.mlkit

splitmix-dieharder: check-dieharder $(BUILDDIR)/splitmix-dieharder
	@echo "Running dieharder test suite. This may take a while."
	time sh -c '$(BUILDDIR)/splitmix-dieharder | $(DIEHARDER) -g 200 -a | tee $(BUILDDIR)/splitmix-dieharder.log'

check-dieharder:
	@if ! command -v $(DIEHARDER) >/dev/null; then \
	    echo "Unable to find '$(DIEHARDER)' on \$$PATH"; \
		return 1; \
	 fi

clean:
	$(RM) $(BUILDDIR)/splitmix-dieharder $(BUILDDIR)/splitmix-dieharder.log
	$(RM) $(BUILDDIR)/hedgehog.poly
	$(RM) -d $(BUILDDIR)
	$(RM) -r hedgehog/.cm/
	$(RM) -r compat/MLB/ hedgehog/MLB/

$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/splitmix-dieharder: test/splitmix-dieharder/sources.mlb test/splitmix-dieharder/test.sml hedgehog/sources.mlb $(SOURCES) | $(BUILDDIR)
	$(MLTON) $(MLTON_FLAGS) -output $@ $<

$(BUILDDIR)/hedgehog.poly: hedgehog/sources.poly $(SOURCES) | $(BUILDDIR)
	echo | $(POLY) $(POLY_FLAGS) -q --error-exit \
		--eval 'val outfile = "$@"' \
		--eval 'use "$<"'

$(BUILDDIR)/hedgehog.mlkit: hedgehog/sources.mlkit.mlb | $(BUILDDIR)
	$(MLKIT) $(MLKIT_FLAGS) --compile_only --output $@ $<
