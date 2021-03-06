/* The following example from the r8169 driver illustrates the REORDER.RW pattern.   

There are 2 state variables involved in this example:

* IntrMask is a hardware register used to diable interrupts
* intr_mask is a software variable that mirrors the value of 
  IntrMask (for reasons related to performance and memory 
  coherency)*/

int IntrMask;
int intr_mask;
int handled;

void thread1() {
while ((IntrMask) == (0)){
}
  if ((intr_mask) == (1)) {
    handled = 1;
  } else {
    handled = 0;
  }
  assert((handled) == (1));
}

void thread2() {
  intr_mask = 1;
  IntrMask = 1;
}



main() {
    IntrMask = 0;
    intr_mask = 0;
    handled = 0;
  thread1();
  thread2();
}

/*
Here, writing the IntrMask variable releases the interrupt thread, 
which checks the intr_mask variable and handles the interrupt, if 
it is non-zero.

Correctness condition here is that the handled flag must eventually 
be set to 1 (i.e., all interrupts must be successfully handled).  
This condition is violated by the following execution:

thread1                        thread2

                                IntrMask = 1
if (intr_mask) --> false
                                intr_mask = 1;


Reordering the two lines labelled (!) and (!!) fixes the problem.

NOTE: In reality threads1 and 2 both run in a loop, where thread1 
releases thread2, which then reenables thread1, and so on.
*/
