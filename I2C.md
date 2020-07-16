I2C words for sys11-forth
=========================

The 68HC11 does not have I2C hardware, so the I2C is bitbanged.

A pair of external MOSFETs are used to drive the lines, so the line states
have to be inverted when written.
The line status can be read at anytime via two inputs.

Only the master mode is supported.

```
Line   SDA   SCL
Output PA4   PA3
Input  PA0   PA1
```

The following words are used:

* @SDA ( -- flag ) Read SDA
* @SCL ( -- flag ) Read SCL
* SDA! ( flag -- ) Write SDA
* SCL! ( flag -- ) Write SCL

* I2C.INIT ( --) (Re-)Initialize the IO port for I2C operation
* I2C.START ( -- ) Start or Restart condition
* I2C.STOP ( -- ) Stop condition
* !I2C ( byte -- ack ) Send a data byte, return ACK
* @I2C ( ack -- byte ) Read a data byte, sending ACK
* N!I2C ( addr len -- flag ) Send N data bytes, return TRUE if all bytes were ACKed
* N@I2C ( adr len -- ) Receive N data bytes, ACK every byte except the last
