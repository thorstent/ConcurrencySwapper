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

void i2c_hid_close () {
  open = (open) - (1);
  if ((open) == (0)) {
    power_on = 0;
  }
  assert((power_on) == (0));
}

void i2c_hid_open() {
  if ((open) == (0)) {
    power_on = 1;
  }
  open = (open) + (1);
  assert((power_on) != (0));
}

void thread_close() {
while (1){
    assume((open) > (0));
    i2c_hid_close();
}
}

void thread_open() {
while (1){
    i2c_hid_open();
}
}



void main() {
    open = 0;
    power_on = 0;
    thread_open();
    thread_close();
}
