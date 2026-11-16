EMACS ?= emacs
SRCS = train-time-pb.el train-time-core.el train-time-gtfs-rt.el train-time-mta-data.el train-time-mta.el train-time.el train-time-map.el train-time-osm.el

.PHONY: all compile test clean data

DATA_DIR ?= build/mta
CURL ?= curl

all: compile test

compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SRCS)

test:
	$(EMACS) -Q --batch -L . -L test -l test/train-time-test.el -l test/train-time-map-test.el -l test/train-time-osm-test.el -l test/train-time-mta-generate-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc

# Regenerate train-time-mta-data.el from MTA's current static data.
data:
	mkdir -p $(DATA_DIR)
	$(CURL) -fsSL -o $(DATA_DIR)/gtfs_subway.zip https://rrgtfsfeeds.s3.amazonaws.com/gtfs_subway.zip
	$(CURL) -fsSL -o $(DATA_DIR)/Stations.csv https://web.mta.info/developers/data/nyct/subway/Stations.csv
	rm -rf $(DATA_DIR)/gtfs && unzip -q -o $(DATA_DIR)/gtfs_subway.zip -d $(DATA_DIR)/gtfs
	$(EMACS) -Q --batch -l scripts/train-time-mta-generate.el -f train-time-mta-generate-batch $(DATA_DIR)/gtfs $(DATA_DIR)/Stations.csv train-time-mta-data.el
