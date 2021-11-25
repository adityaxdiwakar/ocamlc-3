.PHONY: help

help: ## help command for available tasks
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.DEFAULT_GOAL := help


build: ## build to an executable (run with `make run`)
	dune build ./src/ocamlc3.exe

clean: ## clean build artifacts
	dune clean

build-nc: ## clean build (no cache)
	dune clean && dune build ./src/ocamlc3.exe

exec: ## build and execute code
	dune exec ./src/ocamlc3.exe

run: ## run compiled executable
	./_build/default/src/ocamlc3.exe
