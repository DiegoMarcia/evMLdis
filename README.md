# evmdis
This is a minimal disassembler for the Ethereum Virtual Machine bytecode.

## Compiling
Compile command can be found in the [project file](https://github.com/DiegoMarcia/evmdis/blob/master/evmdis.sublime-project).

For instance, I use
```
ocamlc str.cma -annot -o evmdis evmdis.ml
```
No additional dependencies are required (at present time).

## Usage
On the command line, simply type `./evmdis <bytecode>` to see the disassembled bytecode.

For instance,
```
./evmdis 0x600060010100
```
will output
```
PUSH1 0x00
PUSH1 0x01
ADD
STOP
```
Easy as pie.

## Ethereum
More information about Ethereum and the EVM: [http://www.ethdocs.org/en/latest/](http://www.ethdocs.org/en/latest/)

Technical specification can be found in the [Yellow Paper](https://github.com/ethereum/yellowpaper) ([PDF](http://gavwood.com/paper.pdf))
