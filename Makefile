
dependencies: dependencies.json
	@packin install --folder $@ --meta $<
	@ln -snf .. $@/Sequences

test: dependencies
	@$</jest/bin/jest *.jl

.PHONY: test
