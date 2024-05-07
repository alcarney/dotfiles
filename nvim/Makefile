# Makefile for bootstrapping my neovim config on machines without access to
# nix.

SITE_DIR := $(HOME)/.local/share/nvim/site/pack
START_DIR := $(SITE_DIR)/git/start
PLUGINS =

.PHONY: update

default: update

$(SITE_DIR):
	mkdir -p $(SITE_DIR)

# A 'function' for defining a rule that bootstraps a plugin.
define PLUG =
$$(START_DIR)/$1/.git:
	git clone $2 $$(subst .git,,$$@)

PLUGINS += $$(START_DIR)/$1/.git
endef


# To see the result of calling 'PLUG', change eval to info
#
# Common dependencies
$(eval $(call PLUG,everforest,https://github.com/neanias/everforest-nvim))

# Common dependencies
$(eval $(call PLUG,plenary,https://github.com/nvim-lua/plenary.nvim))


# LSP
$(eval $(call PLUG,nvim-lspconfig,https://github.com/neovim/nvim-lspconfig))
$(eval $(call PLUG,nvim-cmp,https://github.com/hrsh7th/nvim-cmp))
$(eval $(call PLUG,cmp-nvim-lsp,https://github.com/hrsh7th/cmp-nvim-lsp))

# "Apps"
$(eval $(call PLUG,telescope,https://github.com/nvim-telescope/telescope.nvim))
$(eval $(call PLUG,toggleterm,https://github.com/akinsho/toggleterm.nvim))

update: $(PLUGINS)
	@for plug in $^ ; do git -C $${plug%.git} pull; done

