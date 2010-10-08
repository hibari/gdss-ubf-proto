###----------------------------------------------------------------------
### Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
###
### Licensed under the Apache License, Version 2.0 (the "License");
### you may not use this file except in compliance with the License.
### You may obtain a copy of the License at
###
###     http://www.apache.org/licenses/LICENSE-2.0
###
### Unless required by applicable law or agreed to in writing, software
### distributed under the License is distributed on an "AS IS" BASIS,
### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
### See the License for the specific language governing permissions and
### limitations under the License.
###
### File    : Makefile
### Purpose : 
###----------------------------------------------------------------------

APPNAME1 = gdss_ubf_proto
NODENAME1 = gdss1
MODULES1 = \
	api_gdss_ubf_proto_init \
	gdss_ubf_proto \
	gdss_ubf_proto_sup \
	ubf_gdss_plugin \
	ubf_gdss_stub_plugin

EUNITTESTS = \
	ubf_gdss_plugin_test \
	ubf_gdss_stub_plugin_test

DISABLEDQUICKTESTS = \
	go_gdss_eqc \
	ubf_gdss_plugin_eqc

PLUGINS = \
	ubf_gdss_plugin \
	ubf_gdss_stub_plugin

# include erl make
include $(shell cat ../.depends-gmt-bom)/make/erl_make.mk

# standard targets
clean::
	rm -rf hlog.* Schema.local

check: all
	rm -rf ./*.log hlog.* Schema.local
	@echo ""
	@echo "NOTE: GDSS shutdown is extremely verbose, check exit status"
	@echo "      for real test results!"
	@echo ""
	@sleep 1
	$(RUNAPP1) -noshell -noinput \
		-s api_gdss_ubf_proto_init simple_internal_setup \
		-s ubf_gdss_plugin_test do_eunit \
		-s ubf_gdss_stub_plugin_test do_eunit \
		-s erlang halt \
		| tee -a check.log

quickcheck: all
	rm -rf ./*.log hlog.* Schema.local
	$(RUNAPP1) -noshell -noinput \
		-s api_gdss_ubf_proto_init simple_internal_setup \
		-s ubf_gdss_plugin_eqc start_test \
		-s eqc stop \
		-s erlang halt \
		| tee -a check.log

interactive: all
	rm -rf ./*.log hlog.* Schema.local
	$(RUNAPP1) \
		-s api_gdss_ubf_proto_init simple_internal_setup
