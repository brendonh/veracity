PKG_NAME = veracity
PKG_PREFIX = veracity

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include
TESTS_DIR = tests

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
TESTS    = $(wildcard $(TESTS_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

ERLC_INCLUDES = -I $(INCLUDE_DIR) 
ERLC_EBINS = $(EBIN_DIR) 

ERLC = erlc
ERLC_OPTS = $(ERLC_INCLUDES) -o $(EBIN_DIR) -Wall -v +debug_info

ERLC_ASN_OPTS = -bber_bin +optimize +compact_bit_string -Wall -v +debug_info
ERLC_ASN = $(ERLC) $(ERLC_ASN_OPTS)

REL_VSN=1
RELEASE_NAME=$(PKG_NAME)-$(REL_VSN)

ERL_CMD=erl \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	-pa $(ERLC_EBINS)

all: $(TARGETS)

run_prereqs: all

$(RELEASE_NAME).boot: $(RELEASE_NAME).rel
	env ERL_LIBS=.. erl -s systools make_script $(RELEASE_NAME) local -s init stop

run: run_prereqs
	$(ERL_CMD)           -sname $(PKG_NAME) -s $(PKG_PREFIX)_app

make test: run_prereqs
	$(ERL_CMD)           -sname $(PKG_NAME) -s $(PKG_PREFIX)_app -s vtest test

daemon: run_prereqs
	$(ERL_CMD) -detached -sname $(PKG_NAME) -s $(PKG_PREFIX)_app >> $(PKG_NAME).log 2>&1

boot: run_prereqs $(RELEASE_NAME).boot
	erl -pa ebin -boot $(RELEASE_NAME) -config $(PKG_NAME) 

release: $(RELEASE_NAME).rel
	env ERL_LIBS=.. erl -s systools make_script $(RELEASE_NAME) -s init stop

stop:
	erl_call -a '$(PKG_NAME)_app stop_and_halt []' -sname $(PKG_NAME)

clean: cleanlog
	rm -f $(TARGETS)
	rm -f $(EBIN_DIR)/*.beam
	rm -f $(RELEASE_NAME).boot $(RELEASE_NAME).script

cleanlog:
	rm -f $(PKG_NAME).log auth.log report.log sasl_err.log erl_crash.dump

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<
