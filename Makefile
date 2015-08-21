# Note: we just dump the defaults in this directory, then
#   make export-default
# to put them in the ~/defaults directory, if desired

ERLC=erlc
ERLC_OPTS=+nowarn_unused_vars +export_all
ERL_SRC = \
	user_default.erl
OBJS := $(patsubst %.erl,%.beam,$(ERL_SRC))

%.beam: %.erl
	@$(ERLC) $(ERLC_OPTS) $<

all: $(OBJS)

clean:
	@rm -f *.beam
