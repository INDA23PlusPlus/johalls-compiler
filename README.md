# johalls-compiler
transpiler from D++ to C

## Usage
Pass the name of the D++ file you wish to compile as the first argument to the program.

For example: `zig build run -- example.dpp`

## Output
The output of the program is the input program transpiled to C, given in stdout.
If there are errors they will be given in stderr.
