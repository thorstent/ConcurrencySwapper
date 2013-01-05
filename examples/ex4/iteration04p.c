#include "havoc.h"
#include "poirot.h"

/* This example is based on a deadlock fixed in the generic USB device driver
 * (git commit id e16362a0c8d90e9adbfe477acbe32b021823fb22).  It illustrates the 
 * REORDER.ADHOC pattern.
 *
 * The fix in this example moves a function call from underneath a lock to avoid lock order 
 * inversion.
 */

/* The deadlock arises because thread1 and thread2 grab two locks in different order. 
 * The bug must be fixed with the following side constraints:  
 * - thread2 code is not part of the driver and cannot be modified
 * - the usb_remove_sysfs_dev_files function is not part of the driver and cannot 
 *   be modified
 * - stuff1() and stuff2() are two "opaque" functions that modify device state 
 *   in ways that we cannot analyse; therefore it is unsafe to remove them from 
 *   underneath user-placed dev_lock
 */
 
 int dev_lock = 0;
 int sysfs_lock = 0;

 void stuff1();
 void stuff2();
 
 int dev_lock_waiting = 0; int sysfs_lock_waiting = 0; 

void usb_remove_sysfs_dev_files() { int thread_id = corral_getThreadID();
  sysfs_lock_waiting = thread_id;
  corral_atomic_begin();
  POIROT_ASSERT(!((((dev_lock) == (thread_id)) && ((dev_lock_waiting) == (sysfs_lock))) && ((sysfs_lock) != (0))));
  corral_atomic_end();
  corral_atomic_begin();
  __hv_assume ((sysfs_lock) == (0));
  sysfs_lock = thread_id;
  sysfs_lock_waiting = 0;
  corral_atomic_end();
  sysfs_lock = 0;
}

 thread2_ns() { int thread_id = corral_getThreadID();
  sysfs_lock_waiting = thread_id;
  corral_atomic_begin();
  POIROT_ASSERT(!((((dev_lock) == (thread_id)) && ((dev_lock_waiting) == (sysfs_lock))) && ((sysfs_lock) != (0))));
  corral_atomic_end();
  corral_atomic_begin();
  __hv_assume ((sysfs_lock) == (0));
  sysfs_lock = thread_id;
  sysfs_lock_waiting = 0;
  corral_atomic_end();
  dev_lock_waiting = thread_id;
  corral_atomic_begin();
  POIROT_ASSERT(!((((sysfs_lock) == (thread_id)) && ((sysfs_lock_waiting) == (dev_lock))) && ((dev_lock) != (0))));
  corral_atomic_end();
  corral_atomic_begin();
  __hv_assume ((dev_lock) == (0));
  dev_lock = thread_id;
  dev_lock_waiting = 0;
  corral_atomic_end();
  stuff2();
  dev_lock = 0;
  sysfs_lock = 0;
}

 thread1() { int thread_id = corral_getThreadID();
  usb_remove_sysfs_dev_files();
  dev_lock_waiting = thread_id;
  corral_atomic_begin();
  POIROT_ASSERT(!((((sysfs_lock) == (thread_id)) && ((sysfs_lock_waiting) == (dev_lock))) && ((dev_lock) != (0))));
  corral_atomic_end();
  corral_atomic_begin();
  __hv_assume ((dev_lock) == (0));
  dev_lock = thread_id;
  dev_lock_waiting = 0;
  corral_atomic_end();
  stuff1();
  dev_lock = 0;
}



void poirot_main() {
	dev_lock = 0;
	sysfs_lock = 0;
	__async_call thread1();
	__async_call thread2_ns();
}
