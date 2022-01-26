# offmt-tool


Offmt, for Offloaded Formatting, is a proof-of-concept project inspired by
[Defmt](https://defmt.ferrous-systems.com/) for Rust or
[Trice](https://interrupt.memfault.com/blog/trice) for C/C++.

The common idea behind those solutions is to move the time and memory consuming
task of formatting text logs from the embedded target to the developer's
computer.

# Trying with the example

To use the example you will need Alire features that are only available in the
development branch, so you have to build it yourself from
[here](https://github.com/alire-project/alire).


## Build the tool

To begin with, we have to build `offmt-tool`:
```console
$ alr build
```

Then go to the example folder:
```console
$ cd example
```

## Instrument

The first step in the process is to instrument the example code.
Instrumentation will replace calls to the Offmt.Log procedure with a series of
other procedure calls sending:
 - The format string ID
 - The potential arguments to be formatted
 - An end-of-frame marker

Instrument the example code using `offmt-tool`:
```console
$ alr exec -- ../bin/offmt-tool -P example.gpr --instrument --output-dir=${PWD}/obj/offmt-instr
```

If you want to have a look at the instrumented code you can use the following
command:
```console
$ alr exec -- gnatpp --pipe obj/offmt-instr/tests.adb
```

`offmt-tool` also produced a TOML file that contains the required data to
reconstruct the format strings: `obj/offmt-instr/offmt_map.toml`.


## Build the instrumented code

A special build switch is required to tell where the instrumented
sources are located:
```console
alr build -- --src-subdirs=offmt-instr
```

## Run the example

If you just run the example as is, you will see garbage on the console. The
`offmt` frames are binary data encoded in COBS, therefore nothing readable will
be printed on the console.

Use hexdump to see the COBS frames (`00` are frame delimiters):
```console
$ ./bin/example | hexdump -C                                                                                                                                                                                                                                                                              [git:main++][15:45]
00000000  02 01 02 2a 01 01 01 00  02 02 02 2b 01 01 01 00  |...*.......+....|
00000010  02 03 02 29 01 01 01 00  02 04 02 29 01 01 01 00  |...).......)....|
00000020  02 05 02 2a 01 00 02 06  02 29 00 02 07 03 a4 01  |...*.....)......|
00000030  00 02 08 02 29 01 01 01  00 02 09 02 2a 01 01 02  |....).......*...|
00000040  29 01 01 02 2b 01 01 01  00 02 0a 01 00 02 0b 02  |)...+...........|
00000050  2a 01 01 01 00 0a                                 |*.....|
00000056
```

## Decode the Offmt frames

To get the actual formatted output, the frames have to be decoded by
`offmt-tool` with the following command:
```console
./bin/example | alr exec -- ../bin/offmt-tool -P example.gpr --decode --output-dir=${PWD}/obj/offmt-instr
```

To have an idea of the gain in throughput, we can compare the size of COBS data:
```console
$ ./bin/example | wc -c
100
```
to the size of formatted output:
```console
$./bin/example | alr exec -- ../bin/offmt-tool -P example.gpr --decode --output-dir=${PWD}/obj/offmt-instr | wc -c
255
```

On top of the reduced CPU time and memory usage, this means that the required
bandwidth for logging is divided by more than 2.5.
