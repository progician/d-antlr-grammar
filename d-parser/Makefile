
# Shell commands
AWK = awk
FIND = /bin/find
MKDIR = mkdir -p
RM = rm -rf
SHELL = /bin/bash

# JVM and JDK setup
JDK_HOME = /c/Program\ Files/Java/jdk1.7.0_03
JAVA = $(JDK_HOME)/bin/java
JAVAC = $(JDK_HOME)/bin/javac
JAR = $(JDK_HOME)/bin/jar

# ANTLR v3 setup
ANTLR3 = antlr-3.4-complete.jar
ANTLR3C = $(JAVA) -jar $(ANTLR3)

# Project values
SOURCES = src
OUTDIR = bin
PROJECT_TITLE = dparser

JFLAGS = \
	-d $(OUTDIR) \
	-source 1.6 \
	-target 1.6

# The main target
all: $(PROJECT_TITLE).jar

# Build all the grammar files in to the their source directory
# $( $(FIND) $(SOURCES) -name '*.g' )
all_grammars := $(shell $(FIND) $(SOURCES) -name '*.g')
g_ext = .g
parser_ext = Parser.java
lexer_ext = Lexer.java
tokens_ext = .tokens
all_parsers := $(subst $(g_ext),$(parser_ext), $(all_grammars) )

$(all_parsers): $(all_grammars)
	$(ANTLR3C) -fo $(dir $@) -make $(subst $(parser_ext),$(g_ext), $@ )
	@echo antlr done

# List the source files in to the file bin/all.sources
all_javas := $(shell $(FIND) $(SOURCES) -name '*.java')
all_classes = $(subst .java,.class,$(all_javas))
all_classes := $(subst $(SOURCES),$(OUTDIR),$(all_classes))
$(all_classes): $(all_parsers) $(all_javas)
	$(JAVAC) $(JFLAGS) -classpath $(ANTLR3) $?

# Build the archive with the main class in the manifest.
$(PROJECT_TITLE).jar: $(all_classes) manifest.txt
	cd $(OUTDIR); $(FIND) . -name '*.class' > classes.list; jar cfm ../$(PROJECT_TITLE).jar ../manifest.txt @classes.list

# Clean-up rule
clean:
	$(RM) $(PROJECT_TITLE).jar
	$(RM) bin/*
	$(RM) $(subst $(g_ext),$(parser_ext), $(all_grammars) )
	$(RM) $(subst $(g_ext),$(lexer_ext), $(all_grammars) )
	$(RM) $(subst $(g_ext),$(tokens_ext), $(all_grammars) )