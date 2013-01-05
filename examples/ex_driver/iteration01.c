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

void drv_xmit_timeout() {
  reset_task_ready = 1;
}

void napi_disable() {
  napi_enabled = 0;
  down(napi_sem);
}

void napi_thread() {
  bool flag;
while (1){
    down(napi_sem);
while (napi_scheduled){
      drv_napi_poll();
}
    up(napi_sem);
}
}

void network_thread() {
  int open1;
  int nondet;
while (1){
    open1 = open();
    if (!(open1)) {
      nondet = poirot_nondet();
while (nondet){
        if (nondet) {
          drv_start_xmit();
        } else {
          drv_xmit_timeout();
        }
        nondet = poirot_nondet();
}
      close();
    }
}
}

void sysfs_thread() {
  int nondet;
while (1){
    lock(sysfs_lock);
    lock(dev_lock);
    if ((sysfs_registered) != (0)) {
      nondet = poirot_nondet();
      drv_sysfs_read(nondet);
    }
    unlock(dev_lock);
    unlock(sysfs_lock);
}
}

void napi_complete() {
  napi_scheduled = 0;
}

void remove_sysfs_files() {
  lock(sysfs_lock);
  device_remove_bin_file();
  unlock(sysfs_lock);
}

void workqueue_thread() {
while (1){
    if (reset_task_ready) {
      drv_reset_task();
      reset_task_ready = 0;
    }
}
}

void drv_reset_task() {
  lock(rtnl_lock);
  if ((netif_running) == (0)) {
    return;
  } else {
    napi_disable();
    dev_down();
    dev_up();
    napi_enable();
    unlock(rtnl_lock);
  }
}

int drv_irq() {
  u8 status;
  int handled = 0;
  status = IntrStatus;
while (status){
    if ((intr_mask) != (0)) {
      write_IntrMask(0);
      intr_mask = 0;
      napi_schedule();
      handled = 1;
    }
    write_IntrStatus(status);
    status = IntrStatus;
}
  return (handled);
}

int close() {
  lock(rtnl_lock);
  if ((netif_running) != (0)) {
    netif_running = 0;
    drv_close();
  }
  unlock(rtnl_lock);
}

int open() {
  int ret = 0;
  lock(rtnl_lock);
  if ((registered) != (0)) {
    ret = drv_open();
    if ((ret) == (0)) {
      netif_running = 1;
    }
  }
  unlock(rtnl_lock);
  return (ret);
}

int drv_open() {
  int rc;
  *(hw_start)();
  IntrMask = 0;
  rc = request_irq();
  if ((rc) < (0)) {
    return (rc);
  } else {
    napi_enable();
    dev_up();
    return (0);
  }
}

void free_irq() {
  irq_enabled = 0;
}

void unregister_netdev() {
  registered = 0;
while ((netif_running) != (0)){
}
}

void device_create_bin_file() {
  sysfs_registered = 1;
}

void drv_napi_poll() {
  int work_done;
  work_done = handle_interrupt();
  if ((work_done) < (100)) {
    napi_complete();
    write_IntrMask(255);
    intr_mask = 255;
  }
}

void drv_hw_start() {
}

void drv_start_xmit() {
}

void drv_disconnect() {
  unregister_netdev();
  remove_sysfs_files();
}

void pci_thread() {
  int probe;
  probe = drv_probe();
  if (probe) {
    return;
  } else {
    lock(dev_lock);
    drv_disconnect();
    unlock(dev_lock);
  }
}

int handle_interrupt() {
  int nondet;
  nondet = poirot_nondet();
  IntrStatus = 0;
  return (nondet);
}

void napi_schedule() {
  atomicStart();
  if ((napi_enabled) != (0)) {
    napi_scheduled = 1;
  }
  atomicEnd();
}

void write_IntrStatus(u8 val) {
  assert((dev_on) != (0));
  IntrStatus = (IntrStatus) & (~(val));
}

void device_remove_bin_file() {
  sysfs_registered = 0;
}

void dev_down() {
  write_IntrMask(0);
  synchronize_irq();
  dev_on = 0;
}

int register_netdev() {
  int nondet;
  nondet = poirot_nondet();
  if (nondet) {
    registered = 1;
    return (0);
  } else {
    return (-(1));
  }
}

void napi_enable() {
  napi_enabled = 1;
}

void dev_up() {
  dev_on = 1;
  write_IntrMask(1);
  intr_mask = 1;
}

void dev_thread() {
while (1){
    IntrStatus = 1;
}
}

int drv_probe() {
  int rc;
  create_sysfs_files();
  rc = register_netdev();
  hw_start = drv_hw_start;
  if ((rc) < (0)) {
    return (rc);
  } else {
    return (0);
  }
}

void write_IntrMask(u8 val) {
  assert((dev_on) != (0));
  IntrMask = val;
}

void drv_close() {
  dev_down();
  napi_disable();
  free_irq();
}

void synchronize_irq() {
  down(irq_sem);
  up(irq_sem);
}

int request_irq() {
  int nondet;
  nondet = poirot_nondet();
  if (nondet) {
    irq_enabled = 1;
    return (0);
  } else {
    return (-(1));
  }
}

void create_sysfs_files() {
  lock(sysfs_lock);
  device_create_bin_file();
  unlock(sysfs_lock);
}

unsigned int drv_sysfs_read(int off) {
  int nondet;
  nondet = poirot_nondet();
  return (nondet);
}

void irq_thread() {
  int handled;
while (1){
    down(irq_sem);
    if ((((irq_enabled) != (0)) && ((IntrStatus) != (0))) && ((IntrMask) != (0))) {
      handled = drv_irq();
      assert((handled) != (0));
    }
    up(irq_sem);
}
}



void main() {
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
	
	pci_thread();
	network_thread();
	irq_thread();
	napi_thread();
	workqueue_thread();
	sysfs_thread();
	dev_thread();
}
