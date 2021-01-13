// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command ccgo is a C compiler producing Go code.
//
// Usage
//
// Invocation
//
//	ccgo { option | input-file }
//
// Environment variables
//
// CCGO_CPP selects which command is used by the C front end to obtain target
// configuration. Defaults to `cpp`.
//
// TARGET_GOARCH selects the GOARCH of the resulting Go code. Defaults to
// $GOARCH or runtime.GOARCH if $GOARCH is not set.
//
// TARGET_GOOS selects the GOOS of the resulting Go code. Defaults to $GOOS or
// runtime.GOOS if $GOOS is not set.
//
// Compiling
//
// To compile for the host invoke something like
//
//	ccgo -o foo.go bar.c baz.c
//
// Cross compiling
//
// To cross compile set TARGET_GOARCH and/or TARGET_GOOS, not GOARCH/GOOS.
// Cross compile depends on availability of C stdlib headers for the target
// platform as well on the set of predefined macros for the target platform.
// For example, to cross compile on a Linux host, targeting windows/amd64, it's
// necessary to have mingw64 installed in $PATH. Then invoke something like
//
//	CCGO_CPP=x86_64-w64-mingw32-cpp TARGET_GOOS=windows TARGET_GOARCH=amd64
//	ccgo -o foo.go bar.c baz.c
//
// Input files
//
// Only files with extension .c or .h are recognized as input files.
//
// Options with arguments
//
// Command line options requiring an argument.
//
// Define a preprocessor macro
//
// -Dfoo
//
// Equals `#define foo 1`.
// 
// -Dfoo=bar
//
// Equals `#define foo bar`.
//
// Setting include search path
//
// -Ipath
//
// Add path to the list of include files search path. The option is a capital
// letter I (India), not a lowercase letter l (Lima).
//
// Undefine a preprocessor macro
//
// -Ufoo
//
// Equals `#undef foo`.
//
// Generating JSON compilation database
//
// -compiledb name
//
// When this option appears anywhere, all preceding options are ignored and all
// following command line arguments are interpreted as a command with arguments
// that will be executed to produce the compilation database. For example:
//
//	ccgo -compiledb compile_commands.json make -DFOO -w
//
// This will execute `make -DFOO -w` and attempts to extract the compile and
// archove commands. 
//
// Only POSIX operating systems are supported.
//
// The only build system supported is `make`.
//
// The only compiler supported is `gcc`.
//
// The only archiver supported is `ar`.
//
// Format specification: https://clang.llvm.org/docs/JSONCompilationDatabase.html
//
// Note: This option produces also information about libraries created with `ar
// cr` and include it in the json file, which is above the specification.
//
// Setting C runtime library import path
//
// -crt-import-path path
//
// Unless disabled by the -nostdlib option, every produced Go file imports the
// C runtime library. Default is `modernc.org/libc`.
//
// Exporting C defines
//
// -export-defines ""
//
// Export C numeric/string defines as Go constants by capitalizing the first
// letter of the define's name.
//
// -export-defines prefix
//
// Export C numeric/string defines as Go constants by prefixing the define's
// name with `prefix`.
//
// Name conflicts are resolved by adding a numeric suffix.
//
// Exporting C enum constants
//
// -export-enums ""
//
// Export C enum constants as Go constants by capitalizing the first letter of
// the enum constant name.
//
// -export-enums prefix
//
// Export C enum constants as Go constants by prefixing the enum constant name
// with `prefix`.
//
// Name conflicts are resolved by adding a numeric suffix.
//
// Exporting C externs
//
// -export-externs ""
//
// Export C extern definitions as Go definitions by capitalizing the first
// letter of the definition name.
//
// -export-externs prefix
//
// Export C extern definitions as Go definitions by prefixing the definition
// name with `prefix`.
//
// Name conflicts are resolved by adding a numeric suffix.
//
// Exporting C struct fields
//
// -export-fields ""
//
// Export C struct fields as Go fields by capitalizing the first letter of the
// field name.
//
// -export-fields prefix
//
// Export C struct fields as Go fields by prefixing the field name with
// `prefix`.
//
// Name conflicts are resolved by adding a numeric suffix.
//
// Exporting tagged C struct and union types
//
// -export-structs ""
//
// Export tagged C struct/union types as Go types by capitalizing the first
// letter of the tag name.
//
// -export-structs prefix
//
// Export tagged C struct/union types as Go types by prefixing the tag name
// with `prefix`.
//
// Name conflicts are resolved by adding a numeric suffix.
//
// Exporting C typedefs
//
// -export-typedefs ""
//
// Export C typedefs as Go types by capitalizing the first letter of the
// typedef name.
//
// -export-structs prefix
//
// Export C typedefs as as Go types by prefixing the typedef name with
// `prefix`.
//
// Name conflicts are resolved by adding a numeric suffix.
//
// Selecting command for target configuration
//
// -host-config-cmd command
//
// This option has the same effect as setting `CCGO_CPP=command`.
//
// Adding options to the configuration command
//
// -host-config-opts comma-separated-list
//
// The separated items of the list are added to the invocation of the
// configuration command.
//
// Setting the Go package name
//
// -pkgname name
//
// Set the resulting Go package name to 'name'. Defaults to `main`.
//
// Compiler scripts
//
// Deprecated, will be removed soon. Use the JSON compile DB instead.
//
// -script filename
//
// Ccgo does not yet have a concept of object files. All C files that are
// needed for producing the resulting Go file have to be compiled together and
// "linked" in memory. There are some problems with this approach, one of them
// is the situation when foo.c has to be compiled using, for example `-Dbar=42`
// and "linked" with baz.c that needs to be compiled with `-Dbar=314`. Or `bar`
// must not defined at all for baz.c, etc.
//
// A script in a named file is a CSV file. It is opened like this (error
// handling omitted):
//
//	f, _ := os.Open(fn)
//	r := csv.NewReader(f)
//	r.Comment = '#'
//	r.FieldsPerRecord = -1
//	r.TrimLeadingSpace = true
//
// The first field of every record in the CSV file is the formal command name
// and is ignored. The remaining fields are the arguments of the ccgo command.
//
// This way different C files can be translated using different options. The
// CSV file may look something like:
//
//	ccgo,-Dbar=42,foo.c
//	ccgo,-Dbar=314,bar.c
//
// Boolean options
//
// Command line options not allowing arguments.
//
// Preprocessing
//
// -E
//
// When this option is present the compiler does not produce any Go files and
// instead prints the preprocessor output to stdout.
//
// Removing error limit
//
// -all-errors
//
// Normally only the first 10 or so errors are shown. With this option the
// compiler will show all errors.
//
// Compiling header files
//
// -header
//
// Using this option suppresses producing of any function definitions. This is
// possibly useful for producing Go files from C header files.
//
// Suppressing C stdlib include search paths
//
// -nostdinc
//
// This option disables the default C include search paths.
//
// Suppressing runtime import
//
// -nostdlib
//
// This option disables importing of the runtime library by the resulting Go
// code.
//
// Undocumented options
//
// There may exist other options not listed above. Those should be considered
// temporary and/or unsupported and may be removed without notice.
// Alternatively, they may eventually get promoted to "documented" options.
package main // import "modernc.org/ccgo/v3"

