
call sbt javah
gcc -shared -o lib-bin/scalaz3.dll -D_JNI_IMPLEMENTATION_ -Wl,--kill-at -I"c:\Program Files\Java\jdk1.7.0_09\include" -I "c:\Program Files\Java\jdk1.7.0_09\include\win32" -I z3\4.0\include src\c\*.h src\c\*.c z3\4.0\bin\z3.lib

copy /y z3\4.0\bin\z3.dll lib-bin
jar cf target\scalaz3_2.9.2-4.0a-win.jar -C target\scala_2.9.2\classes z3 -C . lib-bin\*.dll
