#include "havoc.h"
#include "poirot.h"

/* This example from the r8169 driver illustrates the REORDER.LOCK pattern.

 The example consists of three threads.*/

int napi_poll = 0;
int shutdown = 0;

void stuff1();
void stuff2();
void stuff3();

int napi_poll_waiting = 0; 

void rtl8169_poll() { int thread_id = corral_getThreadID();
  stuff3();
}

void rtl_shutdown() { int thread_id = corral_getThreadID();
  stuff2();
}

void stuff3() { int thread_id = corral_getThreadID();
  POIROT_ASSERT((shutdown) == (0));
}

void stuff2() { int thread_id = corral_getThreadID();
}

void stuff1() { int thread_id = corral_getThreadID();
  shutdown = 1;
}

void thread1() { int thread_id = corral_getThreadID();
  napi_poll_waiting = thread_id;
  corral_atomic_begin();
  corral_atomic_end();
  corral_atomic_begin();
  __hv_assume ((napi_poll) == (0));
  napi_poll = thread_id;
  napi_poll_waiting = 0;
  corral_atomic_end();
  stuff1();
}

void thread3() { int thread_id = corral_getThreadID();
  napi_poll_waiting = thread_id;
  corral_atomic_begin();
  corral_atomic_end();
  corral_atomic_begin();
  __hv_assume ((napi_poll) == (0));
  napi_poll = thread_id;
  napi_poll_waiting = 0;
  corral_atomic_end();
  rtl8169_poll();
  napi_poll = 0;
}

void thread2() { int thread_id = corral_getThreadID();
  rtl_shutdown();
}



void poirot_main() {
	napi_poll = 0;
	shutdown = 0;
	__async_call thread1();
	__async_call thread2();
	__async_call thread3();
}

/*
The above code contains calls to functions stuff1(), stuff2(), 
stuff3().  These functions represent device-specific code that 
accesses device registers.  Since we do not have an accurate 
device model, we have to assume that these functions are not 
commutative and are only safe to execute in the order consistent 
with sequential execution.

Consider lines (1) and (2).  (2) disables the NAPI thread: 
after this lines has been executed, no new calls to the 
rtl8169_poll method will be performed.  In the simplified 
concurrency model the rtl8169_poll entry point cannot be invoked 
while rtl8169_down is running.  As a result, this model rules out 
any execution where stuff3() is called after stuff1().  

In the realistic concurrency model, a call to rtl8169_poll can 
occur between (1) and (2), thus violating the above invariant.  
This race can be eliminated by simply reordering lines (1) and 
(2), which is exactly what the RTL driver does.
*/
