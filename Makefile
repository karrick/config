SHELL := /bin/sh
HOME  ?= $(shell echo ~)

# Symlink pairs: source:destination, both relative to HOME.
# Source is the symlink target; destination is the name of the symlink.
SYMLINKS := \
	.config/.profile:.profile \
	.config/.profile:.zprofile \
	.config/.shrc:.bashrc \
	.config/.shrc:.shrc \
	.config/.shrc:.zshrc

.PHONY: setup check

setup: ## Create shell init symlinks in HOME, backing up existing files to .0
	@set -e; cd "$(HOME)"; \
	for pair in $(SYMLINKS); do \
		src=$${pair%%:*}; \
		dst=$${pair##*:}; \
		if [ -L "$$dst" ] && [ "$$(readlink "$$dst")" = "$$src" ]; then \
			printf "ok      %-20s -> %s\n" "$$dst" "$$src"; \
		elif [ -e "$$dst" ] || [ -L "$$dst" ]; then \
			printf "backup  %-20s -> $$dst.0\n" "$$dst"; \
			mv "$$dst" "$$dst.0"; \
			ln -s "$$src" "$$dst"; \
		else \
			printf "create  %-20s -> %s\n" "$$dst" "$$src"; \
			ln -s "$$src" "$$dst"; \
		fi; \
	done

check: ## Show status of shell init symlinks without making changes
	@cd "$(HOME)"; \
	for pair in $(SYMLINKS); do \
		src=$${pair%%:*}; \
		dst=$${pair##*:}; \
		if [ -L "$$dst" ] && [ "$$(readlink "$$dst")" = "$$src" ]; then \
			printf "ok      %-20s -> %s\n" "$$dst" "$$src"; \
		elif [ -L "$$dst" ]; then \
			printf "wrong   %-20s -> %s (expected %s)\n" "$$dst" "$$(readlink "$$dst")" "$$src"; \
		elif [ -e "$$dst" ]; then \
			printf "file    %-20s (expected symlink to %s)\n" "$$dst" "$$src"; \
		else \
			printf "missing %-20s (expected symlink to %s)\n" "$$dst" "$$src"; \
		fi; \
	done
