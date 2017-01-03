# ------------------
# Makefile to create the FinStress indicator and alternative methods
# Christopher Gandrud
# MIT License
# ------------------

RDIR = .

RSOURCE = $(wildcard $(RDIR)/*.R)

OUT_FILES = $(RSOURCE:.R=.Rout)

all: $(OUT_FILES)

$(RDIR)/%.Rout: $(RDIR)/%.R
	R CMD BATCH $<

clean:
	rm -fv $(OUT_FILES)
