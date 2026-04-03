.PHONY: help
help:  ## Show this help (usage: make help)
	@echo "Usage: make [recipe]"
	@echo "Recipes:"
	@awk '/^[a-zA-Z0-9_.-]+:.*?##/ { \
		helpMessage = match($$0, /## (.*)/); \
		if (helpMessage) { \
			recipe = $$1; \
			sub(/:/, "", recipe); \
			printf "  \033[36m%-20s\033[0m %s\n", recipe, substr($$0, RSTART + 3, RLENGTH); \
		} \
	}' $(MAKEFILE_LIST)

.PHONY: doc
doc:
	dune build @doc-private @doc

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

.PHONY: test-code-exec
test-code-exec:  ## Run code execution tests
	OYSTER_CODE_EXEC_TESTS=true dune test pkg/oystermark/tests/code_exec

.PHONY: setup-hooks
setup-hooks:  ## Install git hooks from scripts/pre-commit
	git config core.hooksPath scripts/pre-commit

.PHONY: devc-up
devc-up:  ## Spin up dev container
	npx @devcontainers/cli up --workspace-folder .

.PHONY: devc-attach
devc-attach:  ## Attach to running dev container
	npx @devcontainers/cli exec --workspace-folder . bash -l

.PHONY: devc-down
devc-down:  ## Kill and remove dev container
	@id=$$(docker ps -q --filter "label=devcontainer.local_folder=$(CURDIR)"); \
	if [ -n "$$id" ]; then \
		docker rm -f $$id; \
	else \
		echo "No running dev container found"; \
	fi

-include Makefile.local
