#include "havoc.h"
#include "poirot.h"

/*  This is a skeleton of the rtl8169 driver.  It contains only code
    related to synchronisation, including all the threads found
    in the real driver (seven threads overall).  It contains 5 
    concurrency bugs, 4 of which can be fixed using the REORDER pattern.  

1. Race condition between drv_up and drv_irq running in the irq_thread.


network_thread                    irq_thread
--------------                    ----------------

|- open
  |- drv_open
     |- dev_up
        |- write_IntrMask(1);
								   |if (IntrMask)
                                   |-drv_irq()
								     | if (!intr_mask)
										|- handled = 0
                                   |- assert(handled != 0)

		|- intr_mask = 1;
		
Possible solution: swap write_IntrMask(1) and intr_mask=1 in the body of dev_up

                                        
2.  Race between interrupt handler and NAPI thread

irq_thread                                      napi_thread
----------                                      -----------

(irq_enabled && IntrStatus && IntrMask)
|- drv_irq
   |- write_IntrMask(0);
   |- intr_mask = 0;
   |- napi_schedule ();
                                                |- drv_napi_poll()
                                                   |- write_IntrMask(1);

(irq_enabled && IntrStatus && IntrMask)
|- drv_irq
   |- intr_mask==0
   |- assert(0)
                                                   |- intr_mask = 1;

Possible solution: swap write_IntrMask() and (intr_mask = 1) in the body 
of drv_napi_poll()


3. Race between driver initialisation function (drv_probe), and network interface
initialisation (drv_open)

pci_thread                                      network_thread
----------                                      --------------

|- drv_probe()
   |- register_netdev()
      |- registered = true
                                                |- open
                                                   |- registered == true
                                                   |- drv_open()
                                                      |- (*hw_start)(); // uninitialised pointer
   |- hw_start = drv_hw_start

Possible solution: swap register_netdev() and (hw_start = drv_hw_start) in drv_probe


4. Race between network_thread and napi_thread

network_thread                                  napi_thread
--------------                                  -----------

|- close()
   |- drv_close()
      |- dev_down()
         |- dev_on = false
                                                |- drv_napi_poll
                                                   |- write_IntrMask(1);
                                                      |- assert(dev_on)  !!!
      |- napi_disable()

Possible solution: swap dev_down and napi_disable in the body of drv_close


5. Deadlock in sysfs_thread

sysfs_thread                                    pci_thread                             
------------                                    ----------

|- lock(sysfs_lock)
                                                |- lock(dev_lock)
                                                |- drv_disconnect
                                                   |- remove_sysfs_files
                                                      |- lock(sysfs_lock);  // DEADLOCK
|- lock(dev_lock)                               
                                                        
Possible solution: change the order the sysfs_thread locks sysfs_lock and dev_lock

*/



typedef unsigned char u8;

typedef int bool;
#define true  1
#define false 0

typedef int mutex_t;

#define locked   1
#define unlocked 0

typedef int semaphore_t;


/* Forward declarations of driver methods */
int  drv_open();
void drv_close();
void drv_disconnect();
int  drv_irq();
void drv_napi_poll();
void drv_start_xmit();
void drv_xmit_timeout();
void drv_reset_task();
unsigned int drv_sysfs_read(int off);

/**************************************************************
 * Environment model.  
 **************************************************************/

/* Global state */
bool registered       = false; // true when the driver interface is open by the OS
bool netif_running    = false; // true after the driver has been registered with the TCP/IP stack

bool irq_enabled      = false;

bool napi_enabled     = false;
bool napi_scheduled   = false;

bool reset_task_ready = false;

bool sysfs_registered = false;

mutex_t dev_lock      = unlocked;
mutex_t sysfs_lock    = unlocked;
mutex_t rtnl_lock     = unlocked;

semaphore_t irq_sem   = 1;
semaphore_t napi_sem  = 1;

u8 IntrStatus;
u8 IntrMask;

/**************************************************************
 * Driver definitions from below  
 **************************************************************/

u8 intr_mask;
bool dev_on    = false;

#define budget 100

void dev_up();
void dev_down();
int  handle_interrupt();
void create_sysfs_files();
void remove_sysfs_files();
void write_IntrMask(u8 val);
void write_IntrStatus(u8 val);
void napi_schedule();
void device_remove_bin_file();
void napi_enable();
void synchronize_irq();

void (*hw_start)();

int hw_start_init = 0; int sysfs_lock_waiting = 0; int napi_sem_waiting = 0; int irq_sem_waiting = 0; int dev_lock_waiting = 0; int rtnl_lock_waiting = 0; 

void drv_xmit_timeout() { int thread_id = corral_getThreadID();
  reset_task_ready = 1;
}

void napi_disable() { int thread_id = corral_getThreadID();
  napi_enabled = 0;
  corral_atomic_begin();
  __hv_assume ((napi_sem) == (0));
  napi_sem = thread_id;
  corral_atomic_end();
}

void napi_thread() { int thread_id = corral_getThreadID();
  bool flag;
  while (1) {
    corral_atomic_begin();
    __hv_assume ((napi_sem) == (0));
    napi_sem = thread_id;
    corral_atomic_end();
    while (napi_scheduled) {
      drv_napi_poll();
    }
    napi_sem = 0;
  }
}

void network_thread() { int thread_id = corral_getThreadID();
  int open1;
  int nondet;
  while (1) {
    open1 = open();
    if (poirot_nondet()) {
    	__hv_assume (!(open1));
      nondet = poirot_nondet();
      while (nondet) {
        if (poirot_nondet()) {
          drv_start_xmit();
        } else {
          drv_xmit_timeout();
        }
        nondet = poirot_nondet();
      }
      close();
    } else {
    	__hv_assume (!(!(open1)));
    }
  }
}

void sysfs_thread() { int thread_id = corral_getThreadID();
  int nondet;
  while (1) {
    corral_atomic_begin();
    __hv_assume ((sysfs_lock) == (0));
    sysfs_lock = thread_id;
    corral_atomic_end();
    corral_atomic_begin();
    __hv_assume ((dev_lock) == (0));
    dev_lock = thread_id;
    corral_atomic_end();
    if (poirot_nondet()) {
    	__hv_assume ((sysfs_registered) != (0));
      nondet = poirot_nondet();
      drv_sysfs_read(nondet);
    } else {
    	__hv_assume (!((sysfs_registered) != (0)));
    }
    dev_lock = 0;
    sysfs_lock = 0;
  }
}

void napi_complete() { int thread_id = corral_getThreadID();
  napi_scheduled = 0;
}

void remove_sysfs_files() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  __hv_assume ((sysfs_lock) == (0));
  sysfs_lock = thread_id;
  corral_atomic_end();
  device_remove_bin_file();
  sysfs_lock = 0;
}

void workqueue_thread() { int thread_id = corral_getThreadID();
  while (1) {
    if (poirot_nondet()) {
    	__hv_assume (reset_task_ready);
      drv_reset_task();
      reset_task_ready = 0;
    } else {
    	__hv_assume (!(reset_task_ready));
    }
  }
}

void drv_reset_task() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  __hv_assume ((rtnl_lock) == (0));
  rtnl_lock = thread_id;
  corral_atomic_end();
  if (poirot_nondet()) {
  	__hv_assume ((netif_running) == (0));
    return;
  } else {
  	__hv_assume (!((netif_running) == (0)));
    napi_disable();
    dev_down();
    dev_up();
    napi_enable();
    rtnl_lock = 0;
  }
}

int drv_irq() { int thread_id = corral_getThreadID();
  u8 status;
  int handled = 0;
  status = IntrStatus;
  while (status) {
    if (poirot_nondet()) {
    	__hv_assume ((intr_mask) != (0));
      write_IntrMask(0);
      intr_mask = 0;
      napi_schedule();
      handled = 1;
    } else {
    	__hv_assume (!((intr_mask) != (0)));
    }
    write_IntrStatus(status);
    status = IntrStatus;
  }
  return (handled);
}

int close() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  __hv_assume ((rtnl_lock) == (0));
  rtnl_lock = thread_id;
  corral_atomic_end();
  if (poirot_nondet()) {
  	__hv_assume ((netif_running) != (0));
    netif_running = 0;
    drv_close();
  } else {
  	__hv_assume (!((netif_running) != (0)));
  }
  rtnl_lock = 0;
}

int open() { int thread_id = corral_getThreadID();
  int ret = 0;
  corral_atomic_begin();
  __hv_assume ((rtnl_lock) == (0));
  rtnl_lock = thread_id;
  corral_atomic_end();
  if (poirot_nondet()) {
  	__hv_assume ((registered) != (0));
    ret = drv_open();
    if (poirot_nondet()) {
    	__hv_assume ((ret) == (0));
      netif_running = 1;
    } else {
    	__hv_assume (!((ret) == (0)));
    }
  } else {
  	__hv_assume (!((registered) != (0)));
  }
  rtnl_lock = 0;
  return (ret);
}

int drv_open() { int thread_id = corral_getThreadID();
  int rc;
  POIROT_ASSERT((hw_start_init) == (1));
  IntrMask = 0;
  rc = request_irq();
  if (poirot_nondet()) {
  	__hv_assume ((rc) < (0));
    return (rc);
  } else {
  	__hv_assume (!((rc) < (0)));
    napi_enable();
    dev_up();
    return (0);
  }
}

void free_irq() { int thread_id = corral_getThreadID();
  irq_enabled = 0;
}

void unregister_netdev() { int thread_id = corral_getThreadID();
  registered = 0;
  while ((netif_running) != (0)) {
  }
}

void device_create_bin_file() { int thread_id = corral_getThreadID();
  sysfs_registered = 1;
}

void drv_napi_poll() { int thread_id = corral_getThreadID();
  int work_done;
  work_done = handle_interrupt();
  if (poirot_nondet()) {
  	__hv_assume ((work_done) < (100));
    napi_complete();
    write_IntrMask(255);
    intr_mask = 255;
  } else {
  	__hv_assume (!((work_done) < (100)));
  }
}

void drv_hw_start() { int thread_id = corral_getThreadID();
}

void drv_start_xmit() { int thread_id = corral_getThreadID();
}

void drv_disconnect() { int thread_id = corral_getThreadID();
  unregister_netdev();
  remove_sysfs_files();
}

void pci_thread() { int thread_id = corral_getThreadID();
  int probe;
  probe = drv_probe();
  if (poirot_nondet()) {
  	__hv_assume (probe);
    return;
  } else {
  	__hv_assume (!(probe));
    corral_atomic_begin();
    __hv_assume ((dev_lock) == (0));
    dev_lock = thread_id;
    corral_atomic_end();
    drv_disconnect();
    dev_lock = 0;
  }
}

int handle_interrupt() { int thread_id = corral_getThreadID();
  int nondet;
  nondet = poirot_nondet();
  IntrStatus = 0;
  return (nondet);
}

void napi_schedule() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  if (poirot_nondet()) {
  	__hv_assume ((napi_enabled) != (0));
    napi_scheduled = 1;
  } else {
  	__hv_assume (!((napi_enabled) != (0)));
  }
  corral_atomic_end();
}

void write_IntrStatus(u8 val) { int thread_id = corral_getThreadID();
  POIROT_ASSERT((dev_on) != (0));
  IntrStatus = (IntrStatus) & (~(val));
}

void device_remove_bin_file() { int thread_id = corral_getThreadID();
  sysfs_registered = 0;
}

void dev_down() { int thread_id = corral_getThreadID();
  write_IntrMask(0);
  synchronize_irq();
  dev_on = 0;
}

int register_netdev() { int thread_id = corral_getThreadID();
  int nondet;
  nondet = poirot_nondet();
  if (poirot_nondet()) {
    registered = 1;
    return (0);
  } else {
    return (-(1));
  }
}

void napi_enable() { int thread_id = corral_getThreadID();
  napi_enabled = 1;
}

void dev_up() { int thread_id = corral_getThreadID();
  dev_on = 1;
  intr_mask = 1;
  write_IntrMask(1);
}

void dev_thread() { int thread_id = corral_getThreadID();
  while (1) {
    IntrStatus = 1;
  }
}

int drv_probe() { int thread_id = corral_getThreadID();
  int rc;
  hw_start_init = 1;
  create_sysfs_files();
  rc = register_netdev();
  if (poirot_nondet()) {
  	__hv_assume ((rc) < (0));
    return (rc);
  } else {
  	__hv_assume (!((rc) < (0)));
    return (0);
  }
}

void write_IntrMask(u8 val) { int thread_id = corral_getThreadID();
  POIROT_ASSERT((dev_on) != (0));
  IntrMask = val;
}

void drv_close() { int thread_id = corral_getThreadID();
  napi_disable();
  dev_down();
  free_irq();
}

void synchronize_irq() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  __hv_assume ((irq_sem) == (0));
  irq_sem = thread_id;
  corral_atomic_end();
  irq_sem = 0;
}

int request_irq() { int thread_id = corral_getThreadID();
  int nondet;
  nondet = poirot_nondet();
  if (poirot_nondet()) {
    irq_enabled = 1;
    return (0);
  } else {
    return (-(1));
  }
}

void create_sysfs_files() { int thread_id = corral_getThreadID();
  corral_atomic_begin();
  __hv_assume ((sysfs_lock) == (0));
  sysfs_lock = thread_id;
  corral_atomic_end();
  device_create_bin_file();
  sysfs_lock = 0;
}

unsigned int drv_sysfs_read(int off) { int thread_id = corral_getThreadID();
  int nondet;
  nondet = poirot_nondet();
  return (nondet);
}

void irq_thread() { int thread_id = corral_getThreadID();
  int handled;
  while (1) {
    corral_atomic_begin();
    __hv_assume ((irq_sem) == (0));
    irq_sem = thread_id;
    corral_atomic_end();
    if (poirot_nondet()) {
    	__hv_assume ((((irq_enabled) != (0)) && ((IntrStatus) != (0))) && ((IntrMask) != (0)));
      handled = drv_irq();
      POIROT_ASSERT((handled) != (0));
    } else {
    	__hv_assume (!((((irq_enabled) != (0)) && ((IntrStatus) != (0))) && ((IntrMask) != (0))));
    }
    irq_sem = 0;
  }
}



void poirot_main() {
	registered       = false; // true when the driver interface is open by the OS
	netif_running    = false; // true after the driver has been registered with the TCP/IP stack

	irq_enabled      = false;

	napi_enabled     = false;
	napi_scheduled   = false;

	reset_task_ready = false;

	sysfs_registered = false;

	dev_lock      = unlocked;
	sysfs_lock    = unlocked;
	rtnl_lock     = unlocked;

	irq_sem   = 0;
	napi_sem  = 0;
	dev_on    = false;
	__async_call pci_thread();
	__async_call network_thread();
	__async_call irq_thread();
	__async_call napi_thread();
	__async_call workqueue_thread();
	__async_call sysfs_thread();
	__async_call dev_thread();
}
