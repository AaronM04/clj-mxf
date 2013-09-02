build:
	if ! which lein; then sudo cp -p support/lein /usr/local/bin; fi
	lein uberjar

install:
	cp target/clj-mxf-`head -1 < project.clj | awk '{print $3}' | tr -d '"'`-standalone.jar /usr/local/bin
# TODO: install shell scripts
