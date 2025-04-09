About Yoctopuce examples for Lazarus
====================================

## Windows Systems
On windows, you have to copy yapi.dll (32bit version) 
and yapi64.dll (64bit version) next to your executable.
You can use the provided `copydll.bat` script to copy
them.

## Linux Systems
On linux systems you will need  the following libs
*libyapi-i386.so  for Intel 32 bits systems
*libyapi-amd64.so for  Intel 64 bits systems
*libyapi-armhf.so for  ARM 32 bits systems
*libyapi-aarch64.so for ARM 64 bits systems
These are located in the `Source/dll` folder

You have to make sure that Lazarus will find them at
compilation time and your executable will find it at
execution time

The simplest way to do that is to copy all of them
in  `/usr/lib` for instance, you can use the following
command:
``sudo cp ../../../Sources/dll/libyapi*.so  /usr/lib/.``

Alternatively, you can copy the required lib files next
to your main source code and set your `LD_LIBRARY_PATH` to
the current directory with the following command:
``export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH``




  
