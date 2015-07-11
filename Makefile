dependencies:
	@mkdir -p $@/jkroso
	@ln -snf ../.. $@/jkroso/prospects

test: dependencies
	@jest index.jl

.PHONY: test
