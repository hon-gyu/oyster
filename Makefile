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

.PHONY: build-doc
build-doc:
	dune build @doc-private @doc

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

.PHONY: setup-hooks
setup-hooks:  ## Install git hooks from scripts/pre-commit
	git config core.hooksPath scripts/pre-commit

-include Makefile.local
