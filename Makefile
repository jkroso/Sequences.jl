dependencies: index.jl
	@kip index.jl
	@ln -snf .. $@/Sequences

test: dependencies
	@jest index.jl

.PHONY: test
