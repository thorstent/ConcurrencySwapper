Introduction
============

This software is to be used to fix concurrency bugs in C code. It works by using [Poirot](http://research.microsoft.com/en-us/projects/poirot/) to discover a bug. The bug is then fixed by either reordering lines of code in a way that it makes no difference sequentially to the program or by placing atomic sections. For details please refer to the paper...

Contact
-------

If you want to contact me feel free to write me at
thorstentXliveDcom (X=@, D=.)

Examples
--------

Please check out the examples folder to see the examples and the changes the software does on them. This allows you to get an impression how it works without having to compile and run the software on your machine.

Installation
============

We assume that you have Windows 7 x64 running. This is mainly due to Poirot being a windows tool. You may however run it in Wine, but it will be slow.

Running the software itself is not very hard, but we have some prerequisites. Unfortunately I cannot post them here on github, but if you wish to get a *precompiled version of the dependencies* please drop me an email.

Java and Scala
--------------

Please install the java 7 x86 JDK on your system as well as Scala 2.9.

ScalaZ3
-------

In order for Z3 to be invoked we use the ScalaZ3 library. Please clone my [fork of this library](https://github.com/thorstent/ScalaZ3) because this is known to work. Even though my changes were reintegrated into the main fork there seem to be some later changes breaking something. Once cloned you also need to install SBT according to [these instructions](http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.7.jar).

You then place the following files from Z3 into these folders. If you have no precompiled version of Z3 (32 bit version) you will need to compile it yourself.
ScalaZ3\z3\4.0\bin: z3.dll and z3.lib
ScalaZ3\z3\4.0\include: z3.h and z3_*.h

You will find the build file for windows in the scalaz3 folder of this git repo. Copy it to your ScalaZ3 repo and build ScalaZ3 with it.

Copy the resulting scalaz3_2.9.2-4.0a-win.jar from the target directory to the lib\z3 directory of this repo.

*If you rebuild don't forget to delete the temporary ScalaZ3 folder*: C:\Users\***\AppData\Local\Temp\SCALAZ3_

Cpachecker
----------

We need a few libraries from the Cpachecker as well as Cpachecker itself. Be sure to [download Cpachecker 1.1](http://cpachecker.sosy-lab.org/#download) as we are not compatible with newer versions. We need these libraries to be put into lib:
* cpachecker.jar
* eclipse-cdt6-parser.jar
* guava-10.0.jar
* javabdd-1.0b2.jar
* java-cup-11a.jar
* jgrapht-jdk1.6.jar
* json_simple-1.1.jar
* jsr305-1.3.9.jar
* sigar.jar
* yicesapijava.jar

as well as the complete eclipse folder.

Poirot4C
--------

As a precondition you need to the [Windows 7.1 SDK](http://www.microsoft.com/en-us/download/details.aspx?id=8279). But you only need to install the MS C Compiler and nothing else. If the installation of the SDK causes problems try to uninstall the Visual C++ 2010 Redistributable, as a newer version of these can block the installation.

Then download and install [Poirot4C](http://research.microsoft.com/en-us/downloads/220b60b2-c309-4ae7-8e86-a3232a2c01f4/default.aspx). The installation procedure is described in the readme file. Be sure to set the POIROT_HOME variable globally as we use it.

In the Poirot installation folder be sure to rename ConcurrencyExplorer.exe to ConcurrencyExplorer1.exe to stop Poirot from showing this every time.

IntelliJ 12
-----------

Please download and install [IntelliJ 12 Community Edition](http://www.jetbrains.com/idea/download/index.html). Be sure to install the Scala Plugin after downloading. We use IntelliJ to build and run the code.

Using the software
==================
We generally run the software from the folder where the C file to analyze is located. The c file itself is the command line argument. For example run from the folder examples and give it ex3.c as an argument.

It will create two subfolders during the run PoirotStage and output.

*output* is used by both Cpachecker and us. There you will find all the control flow automatons (CFA) produced by Cpachecker during parsing. Also written there is the program that is discharged during every iteration to Poirot and in the end the correctProgram.c. For the last iteration there is the counterexamples (ctex?.txt) and the formulalog.txt that is used for debug purposes (you need to stop the program in the debugger to prevent them from being overwritten in the next iteration. The formulalog contains all the formulas generated for commands and discharged to Z3 as well as the answer Z3 provided. Lastly order.dot contains a diagram of dependencies between instructions in the c file. This file is also overwritten after each iteration as new dependencies are added to switch instructions.

The Poirot stage folder contains all the file needed to run Poirot and the output Poirot produces. The folder is emptied completely before each iteration. If you want to debug Poirot you can invoke it with analysis.bat in that folder. If there are errors with the build run nmake to see details.

*For a detailed description of the output look at the examples folder.*