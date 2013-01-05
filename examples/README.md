Examples
========

This folder contains six examples that we used to test our prototype. The first four are simpler ones that demonstrate one scenario that we manage to fix by swapping. The fifths is fixed by placing an atomic section. The driver example is a simplified driver code that contains 5 bugs that are fixed one after another.

For each example the corrected file is also provided with the ending "-corrected.c". However, it is not suitable for comparison with the original file because our C code printer prints functions in a different order and with different formatting than the original code. It is best to compare the iterations with each other (see below).

The output.txt
--------------

The output.txt file contains the command line output of our test suite that processes all the examples in one run. The command line output first states which file is being worked on. It then lists the individual iterations. For each iteration that produced a counter-example the length of the counter-examples in lines is listed. If an iteration produced no counter-example we know that this phase is finished as the program contains no more bugs.

The program works in two phases. In the first phase it simply only looks at the asserts that are already part of the program and it will swap lines or place locks until Poirot no longer finds any bugs. When this phase is finished it prints *"Starting deadlock analysis"*. The second phase adds additional assertions when locks are used to verify that no deadlocks can occur. If Poirot then finds new assertion violations these are used to swap more lines until these are again fixed.
The reason we work in two phases is because the deadlock analysis adds several assertions that slow down Poirot.

The output also indicates which *iterations fixed a bug*. This is a very important notion because a counter-example can lead to a number of possibilities to swap lines and those combinations can again lead to a number of other combinations to try (we use a breath-first search). A bug is considered fixed if Poirot finds a different assertion that fails or the threads leading to the failed assertion are different. Once a bug is fixed all other combinations are discarded and we focus on the new bug. This saves time because Poirot takes long to try each combination. To compare iterations one should compare each iteration to the last iteration that fixed a bug, because that iteration is the baseline for all combinations until the next bug is fixed.

Lastly the string *"dead end"* after an iteration indicates that the bug was not fixed, but the counter-example cannot be used to learn something new. It generally means that the bug would occur also in a sequential setting.

The output_old.txt
--------------

The output_old.txt was the output before the implementation of the partial order traces. It is provided for comparison and has the same structure as the output.txt.

The output folders
==================

For each example we generate an output folder with the name of the example file. The folder contains the counter-examples and the files discharged to Poirot. For each iteration we print the file iterationXX.c which contains the C code at the beginning of the iteration. iteration01.c contains the same code as the original example, but printed with our printer, so it is a good baseline for comparison. iterationXXp.c contains the code discharged to Poirot, which has Poirot specific constructs (mainly for locks).

The counter-examples
--------------------

ctexXX.txt is a print-out of the counter-example after that iteration. The information gathered from the counter-example is used to decide which lines to switch for the next iteration. The format is:

threadnumber: stacktrace (current stackframe leftmost): instruction

for example: "2: create_sysfs_files, drv_probe, pci_thread: lock(sysfs_lock)"

The line beginning with an exclamation mark (!) is the line that holds the assertion that failed. The lines beginning with "l" are lines that were added to the counter-example by the analysis engine. The problem is that the original counter-example does not continue most threads after the assertion failed. However, for our counter-example analysis we need to know which line would have come next in order to swap lines. Because of that for each thread in the counter-example we add the line that had have been executed next if the program would not have terminated due to the assertion failure.