###----------------------------------------------------------------------
### Copyright (c) 2007-2015 Hibari developers.  All rights reserved.
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

UBF_CLASSES_DIR = ../../../../erl-third-party/sics/ubf__HEAD/priv/java/classes

all:
	@echo "Edit the UBF_CLASSES_DIR in this Makefile to contain the "
	@echo "path to the UBF package's priv/java/classes subdirectory."
	@echo ""
	@echo "Then run 'make HibariTest' and 'make run-HibariTest'"
	@echo ""
	@exit 1

HibariTest:
	javac -d classes -classpath classes:src:$(UBF_CLASSES_DIR) \
	    src/ubf/HibariTest.java

run-HibariTest:
	java -cp classes:$(UBF_CLASSES_DIR) ubf.HibariTest localhost
