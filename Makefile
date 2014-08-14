PROJECT = propagator
PROJECT_VERSION = 0.1

TEST_DEPS = nifoc_ct_helper
dep_nifoc_ct_helper = git https://github.com/nifoc/nifoc_ct_helper master

ifeq ($(USER),travis)
	TEST_DEPS += ecoveralls
	dep_ecoveralls = git https://github.com/nifoc/ecoveralls master
endif

ERLC_OPTS ?= -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

TEST_ERLC_OPTS ?= +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

CT_SUITES = group process
CT_OPTS = -ct_hooks nifoc_ct_hook [] -cover ./test/cover.spec

EDOC_OPTS = {def, [ \
					{years, "2014"}, \
					{version, "$(PROJECT_VERSION)"} \
				]}

include erlang.mk

coverage-report: $(shell ls -1rt `find logs -type f -name \*.coverdata 2>/dev/null` | tail -n1)
	$(gen_verbose) erl -noshell -pa ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

upload-docs: docs
	$(gen_verbose) rsync -avz --no-o --no-g -e ssh --chmod=og=r -p --delete --exclude '*.edoc' --exclude 'edoc-info' doc/ kempkens:/var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)
	@ssh kempkens chown -R www-data:www-data /var/www/nifoc/$(PROJECT)/$(PROJECT_VERSION)

.PHONY: coverage-report upload-docs
