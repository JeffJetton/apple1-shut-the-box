# Apple 1 "Shut the Box"
    
A version of the traditional pub game [Shut the Box](https://en.wikipedia.org/wiki/Shut_the_Box), written in assembly for the Apple 1 and its various clones/emulators.

Exactly 1K in size.

<img src="https://github.com/JeffJetton/apple1-shut-the-box/blob/master/img/screenshot.png" width="400">


## Files in the Repo:

* **shut.asm**
    * The 6502 assembly language source code
    * I used [dasm](https://dasm-assembler.github.io/) to assemble this, but it should work with other assemblers (perhaps with a few changes).
           
* **shut.bin**
    * Assembled binary file
    * This is dasm's default output format, so the first two bytes are the (little-endian) origin address of the code, and not the actual code itself.
    
* **shut.js**
    * Javascript "tape file" format, compatible with Will Scullin's [Apple 1js emulator](https://www.scullinsteel.com/apple1/)
    * You can use this with a local copy of the emulator by putting the file in the `/tapes` directory and adding a reference to it in `apple1.htm`
    
* **shut.txt**
    * The program represented in typed out "Woz Monitor" format
    * Many emulators will let you copy/paste or otherwise load this in.
    * In theory, you could also send this over to a real Apple 1 (or replica/clone) via serial communication.