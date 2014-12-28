
dependencies: dependencies.json
	@packin install --folder $@ --meta $<
	@ln -snf .. $@/prospects

test: dependencies
	@$</jest/bin/jest test

.PHONY: test
