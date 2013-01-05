#include "havoc.h"
#include "poirot.h"

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

int hw_start_init = 0; int init_sem_waiting = 0; 

int rtl8169_open() { int thread_id = corral_getThreadID();
  POIROT_ASSERT((hw_start_init) == (1));
}

int register_netdev() { int thread_id = corral_getThreadID();
  init_sem = 0;
}

int start_device() { int thread_id = corral_getThreadID();
  return (0);
}

int thread1() { int thread_id = corral_getThreadID();
  register_netdev();
  hw_start_init = 1;
}

int thread2() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  __hv_assume ((init_sem) == (0));
  init_sem = thread_id;
  corral_atomic_end();
  rtl8169_open();
}



void poirot_main() {
	init_sem = 1;
	__async_call thread1();
	__async_call thread2();
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
