#include "havoc.h"
#include "poirot.h"

/* Example based on a bug in the i2c-hid driver.
 * This example illustrates the LOCK pattern.
 *
 * commit# 7a7d6d9c5fcd4b674da38e814cfc0724c67731b2
 */

/* Error scenario:
 *
 * open_thread                    close_thread
 * -----------                    ------------
 * (open == 0) --> yes
 * power_on=1
 * open++ // open=1
 *                                open-- // open=0
 *                                (open==0) --> yes
 * (open == 0) --> yes
 * power_on=1
 * open++ // open=1
 *                                power_on=0
 * assert (power_on) // ERROR
 */

/* One possible fix is to put locks around the body of i2c_hid_open() or i2c_hid_close().
 */

int open = 0;
int power_on = 0;



void i2c_hid_close () { int thread_id = corral_getThreadID();
  open = (open) - (1);
  if (poirot_nondet()) {
  	__hv_assume ((open) == (0));
    power_on = 0;
  } else {
  	__hv_assume (!((open) == (0)));
  }
  POIROT_ASSERT((power_on) == (0));
}

void i2c_hid_open() { int thread_id = corral_getThreadID();
  if (poirot_nondet()) {
  	__hv_assume ((open) == (0));
    power_on = 1;
  } else {
  	__hv_assume (!((open) == (0)));
  }
  POIROT_ASSERT((power_on) != (0));
  open = (open) + (1);
}

void thread_close() { int thread_id = corral_getThreadID();
  while (1) {
    __hv_assume((open) > (0));
    i2c_hid_close();
  }
}

void thread_open() { int thread_id = corral_getThreadID();
  while (1) {
    i2c_hid_open();
  }
}



void poirot_main() {
    open = 0;
    power_on = 0;
	__async_call thread_open();
	__async_call thread_close();
}
