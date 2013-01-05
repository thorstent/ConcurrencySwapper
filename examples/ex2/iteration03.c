/*This example from the r8169 driver illustrates the REORDER.RELEASE 
  pattern.

The example involves two driver functions.  The first one (thread1()) is the 
initialisation function invoked when the driver is loaded.  It 
allocates various resources required by the driver and registers 
the driver in the system using the register_netdev function. 
The second function rtl8169_open is invoked when a user-level program
wants to start using the device. */

/*OS model:*/

int init_sem = 1;
int (*hw_start)() = 0;

int rtl8169_open() {
  *(hw_start)();
}

int register_netdev() {
  up(init_sem);
}

int start_device() {
  return (0);
}

int thread1() {
  hw_start = &(start_device);
  register_netdev();
}

int thread2() {
  down(init_sem);
  rtl8169_open();
}



void main() {
	init_sem = 1;
	thread1();
	thread2();
}

/*The problem in the above code is that the rtl8169_open function is
invoked by a separate thread released by the register_netdev 
function.  As a result, the tp->hw_start pointer can be used 
before being initialised.
*/

/*
Problematic execution:

thread1                    thread2
----------------                    -------------

call register_netdev
up(init_sem)                        down(init_sem)
                                    call rtl8169_open
                                    tp->hw_start() !!! DEREFERENCING UNINITIALISED POINTER
tp->hw_start = cfg->hw_start

The bug can be fixed by swapping lines (1) and (2) in the implementation 
of thread1.
*/
