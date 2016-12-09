ERLC       = erlc
BEAM_SHELL = erl
LONGNAME   = brinley@127.0.0.1
BEAM_FLAGS = -name $(LONGNAME) -setcookie cookie

ERL_SRCS  = $(shell echo *.erl)

all: compile

runmonitor: compile
	$(BEAM_SHELL) $(BEAM_FLAGS) -run start_monitor

runpeer: compile
	$(BEAM_SHELL) $(BEAM_FLAGS) -run start_client 

compile: $(ERL_SRCS)
	$(ERLC) $^

clean:
	rm -f *.beam