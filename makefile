.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = client flags msg timer msg_reader msg_writer

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:
	make compile
	make

client.beam: client.erl
	${ERL} -W0 client.erl

flags.beam: flags.erl
	${ERL} -W0 flags.erl

timer.beam: timer.erl
	${ERL} -W0 timer.erl

msg.beam: msg.erl
	${ERL} -W0 msg.erl

msg_reader.beam: msg_reader.erl
	${ERL} -W0 msg_reader.erl

msg_writer.beam: msg_writer.erl
	${ERL} -W0 msg_writer.erl

client: compile
	${ERL} -noshell -s client

clean:
	rm -rf *.beam erl_crash.dump
