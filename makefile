SFLAGS = --compile-imported-libraries --optimize-level 3
Scheme = boot/rcis
Ver = a6le
Pos = $(PWD)/boot/${Ver}
# if this can not be executed in your device then install the chezscheme and change S into 'scheme'
S = ${Pos}/scheme -b ${Pos}/petite.boot -b ${Pos}/scheme.boot

defalut: scripts/boot.ss clean-exec init
	@bash scripts/boot.sh $S $< -q  # makeing boot file -> boot/rcis.boot
	@((test -f $(PWD)/${Scheme}.boot) || (printf "\r* booting file...\033[31;1mfailed\033[0m\n" && exit 1)) && \
		printf "\r* booting file...ok\n";
	@echo "#! /bin/bash" >> ${Scheme}
	@echo '$S --program ${PWD}/${Scheme}.boot "$$@"' >> ${Scheme}
	@chmod 555 ${Scheme}
	@((test -f ${Scheme}) || (printf "\r* produce executable...\033[31;1mfailed\033[0m\n" && exit 1)) && \
		printf "\r* produce executable...ok\n";
	@(echo 'Executable at $(Scheme)')

install: scripts/boot.ss clean-exec init
	@bash scripts/boot.sh scheme $< -q  # makeing boot file -> boot/rcis.boot
	@((test -f $(PWD)/${Scheme}.boot) || (printf "\r* booting file...\033[31;1mfailed\033[0m\n" && exit 1)) && \
		printf "\r* booting file...ok\n";
	@echo "#! /bin/bash" >> ${Scheme}
	@echo 'scheme --program ${PWD}/${Scheme}.boot "$$@"' >> ${Scheme}
	@chmod 555 ${Scheme}
	@((test -f ${Scheme}) || (printf "\r* produce executable...\033[31;1mfailed\033[0m\n" && exit 1)) && \
		printf "\r* produce executable...ok\n";
	@(echo 'Executable at $(Scheme)')

cors: scripts/start.ss compiler/cors.ss init
	@mkdir -p build/
	@$S $<

test: scripts/test.ss init
	@mkdir -p build/
	@$S $<

test-all: init
	@mkdir -p build/
	@echo "(output-file \"build/t\")) (test-all)" | $S scripts/test.ss -q

test-%: init
	@mkdir -p build/
	@echo "(output-file \"build/t\")) (scheme-optimize-level \"$*\") (test-all)" | $S scripts/test.ss -q

test-driver:  init
	@mkdir -p build/
	@echo "(output-file \"build/t\") (test-driver)" | $S scripts/test.ss -q

test-driver-%: init
	@mkdir -p build/
	@echo "(output-file \"build/t\")) (scheme-optimize-level \"$*\") (test-driver)" | $S scripts/test.ss -q

init: compiler/options.in
	@if ([ compiler/options.in -nt compiler/options.ss ] || [ ! -f compiler/options.ss ]); \
	then \
		echo -n "* configure options.ss..."; \
		cp $< compiler/options.ss; \
		sed -i 's|__PWD__|$(PWD)|g' compiler/options.ss; \
		([ -f compiler/options.ss ] || (echo "\033[31;1mfailed\033[0m" && exit 1)) && \
		printf "\r* configure options.ss...ok\n"; \
	fi

.PHONY: clean
cl: clean
clean: clean-exec clean-so
	@$(RM) -rf build/ compiler/options.ss
clean-exec:
	@$(RM) -f ${Scheme}.boot ${Scheme}
clean-so:
	@$(RM) -r */*.so
