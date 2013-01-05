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
 
 void usb_remove_sysfs_dev_files() {
  lock(sysfs_lock);
  unlock(sysfs_lock);
}

 thread2_ns() {
  lock(sysfs_lock);
  lock(dev_lock);
  stuff2();
  unlock(dev_lock);
  unlock(sysfs_lock);
}

 thread1() {
  usb_remove_sysfs_dev_files();
  lock(dev_lock);
  stuff1();
  unlock(dev_lock);
}



void main() {
	dev_lock = 0;
	sysfs_lock = 0;
	thread1();
	thread2_ns();
}
