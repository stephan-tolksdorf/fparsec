
CSC:= dmcs
CSC_FLAGS_COMMON:= /noconfig /codepage:utf8 /warn:4 /nowarn:1591 /unsafe /define:CLR4,LOW_TRUST
CSC_FLAGS_DEBUG:= $(CSC_FLAGS_COMMON) /debug:full /optimize- /define:DEBUG
CSC_FLAGS_RELEASE:= $(CSC_FLAGS_COMMON) /debug /debug:pdbonly /optimize+

FSC:= fsharpc
FSC_FLAGS_COMMON:= /warn:4 /tailcalls- /define:LOW_TRUST
FSC_FLAGS_DEBUG:= $(FSC_FLAGS_COMMON) /debug:full /optimize- /define:DEBUG /define:NOINLINE /define:DEBUG_STATIC_MAPPING
FSC_FLAGS_RELEASE:= $(FSC_FLAGS_COMMON) /debug /debug:pdbonly /optimize+

DEBUG_DIR:= bin/Debug
RELEASE_DIR:= bin/Release

FPARSECCS_REFS:= /r:System.dll /r:System.Core.dll /r:FSharp.Core.dll
FPARSECCS_SRCS:=\
 ../FParsecCS/Properties/AssemblyInfo.cs \
 ../FParsecCS/Buffer.cs \
 ../FParsecCS/CaseFoldTable.cs \
 ../FParsecCS/CharSet.cs \
 ../FParsecCS/CharStream.cs \
 ../FParsecCS/CharStreamLT.cs \
 ../FParsecCS/Cloning.cs \
 ../FParsecCS/ErrorMessage.cs \
 ../FParsecCS/ErrorMessageList.cs \
 ../FParsecCS/Errors.cs \
 ../FParsecCS/FastGenericEqualityERComparer.cs \
 ../FParsecCS/HexFloat.cs \
 ../FParsecCS/IdentifierValidator.cs \
 ../FParsecCS/ManyChars.cs \
 ../FParsecCS/OperatorPrecedenceParser.cs \
 ../FParsecCS/Position.cs \
 ../FParsecCS/Reply.cs \
 ../FParsecCS/StringBuffer.cs \
 ../FParsecCS/Strings.cs \
 ../FParsecCS/Text.cs \
 ../FParsecCS/UnmanagedMemoryPool.cs

FPARSEC_REFS:= /r:System.dll /r:System.Core.dll
FPARSEC_SRCS:=\
 ../FParsec/AssemblyInfo.fs \
 ../FParsec/Internals.fs \
 ../FParsec/Range.fs \
 ../FParsec/Emit.fs \
 ../FParsec/StaticMapping.fsi \
 ../FParsec/StaticMapping.fs \
 ../FParsec/Error.fsi \
 ../FParsec/Error.fs \
 ../FParsec/Primitives.fsi \
 ../FParsec/Primitives.fs \
 ../FParsec/CharParsers.fsi \
 ../FParsec/CharParsers.fs

TEST_SRCS:=\
 ../Test/Test.fs \
 ../Test/BufferTests.fs \
 ../Test/CharSetTests.fs \
 ../Test/HexFloatTests.fs \
 ../Test/TextTests.fs \
 ../Test/CloningTests.fs \
 ../Test/StringBufferTests.fs \
 ../Test/CharStreamTests.fs \
 ../Test/PrimitivesTests.fs \
 ../Test/CharParsersTests.fs \
 ../Test/IdentifierValidatorTests.fs \
 ../Test/OperatorPrecedenceParserTests.fs \
 ../Test/RangeTests.fs \
 ../Test/StaticMappingTests.fs \
 ../Test/AllTests.fs

all: debug release

mk_debug_dir:
	@mkdir -p $(DEBUG_DIR)

mk_release_dir:
	@mkdir -p $(RELEASE_DIR)

$(DEBUG_DIR)/FParsecCS.dll: $(FPARSECCS_SRCS)
	$(CSC) /target:library /out:$(DEBUG_DIR)/FParsecCS.dll $(CSC_FLAGS_DEBUG) $(FPARSECCS_REFS) $(FPARSECCS_SRCS)

$(RELEASE_DIR)/FParsecCS.dll: $(FPARSECCS_SRCS)
	$(CSC) /target:library /out:$(RELEASE_DIR)/FParsecCS.dll $(CSC_FLAGS_RELEASE) $(FPARSECCS_REFS) $(FPARSECCS_SRCS)

$(DEBUG_DIR)/FParsec.dll: $(DEBUG_DIR)/FParsecCS.dll $(FPARSEC_SRCS)
	$(FSC) /target:library /out:$(DEBUG_DIR)/FParsec.dll $(FSC_FLAGS_DEBUG) /r:$(DEBUG_DIR)/FParsecCS.dll $(FPARSEC_REFS) $(FPARSEC_SRCS)

$(RELEASE_DIR)/FParsec.dll: $(RELEASE_DIR)/FParsecCS.dll $(FPARSEC_SRCS)
	$(FSC) /target:library /out:$(RELEASE_DIR)/FParsec.dll $(FSC_FLAGS_RELEASE) /nooptimizationdata /r:$(RELEASE_DIR)/FParsecCS.dll $(FPARSEC_REFS) $(FPARSEC_SRCS)

$(DEBUG_DIR)/test_fparsec.exe: $(DEBUG_DIR)/FParsec.dll $(TEST_SRCS)
	$(FSC) /target:exe /out:$(DEBUG_DIR)/test_fparsec.exe $(FSC_FLAGS_DEBUG) /r:$(DEBUG_DIR)/FParsecCS.dll /r:$(DEBUG_DIR)/FParsec.dll $(FPARSEC_REFS) $(TEST_SRCS)

$(RELEASE_DIR)/test_fparsec.exe: $(RELEASE_DIR)/FParsec.dll $(TEST_SRCS)
	$(FSC) /target:exe /out:$(RELEASE_DIR)/test_fparsec.exe $(FSC_FLAGS_DEBUG) /r:$(RELEASE_DIR)/FParsecCS.dll /r:$(RELEASE_DIR)/FParsec.dll $(FPARSEC_REFS) $(TEST_SRCS)


debug: mk_debug_dir $(DEBUG_DIR)/FParsec.dll $(DEBUG_DIR)/test_fparsec.exe

release: mk_release_dir $(RELEASE_DIR)/FParsec.dll $(RELEASE_DIR)/test_fparsec.exe

clean:
	-rm -r $(DEBUG_DIR)
	-rm -r $(RELEASE_DIR)
