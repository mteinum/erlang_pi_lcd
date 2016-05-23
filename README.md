# Adafruit Blue&White 16x2 LCD+Keypad Kit for Raspberry Pi

Erlang/OTP Application for the 16x2 LCD with [MCP23017](http://ww1.microchip.com/downloads/en/DeviceDoc/21952b.pdf) on Raspberry Pi.

This is a port of the Python source code to Erlang.

* MCP code inspired by [MCP230xx](https://github.com/adafruit/Adafruit_Python_GPIO/blob/master/Adafruit_GPIO/MCP230xx.py)
* LCD code inspired by [Adafruit_CharLCD](https://github.com/adafruit/Adafruit_Python_CharLCD/blob/master/Adafruit_CharLCD/Adafruit_CharLCD.py)

> At this time, the code and plate can control the white backlight on or off. There is no support for PWM control of the backlight at this time, so if you need to have more granular control of the backlight, this plate can't do that (the I2C expander does not have PWM output).

For more information on the product, prices and distributors, visit the [Adafruit product page](https://www.adafruit.com/products/1115).

I got mine from [Kjell and Company](http://www.kjell.com/no/produkter/data-og-nettverk/enkortsdata/raspberry-pi/adafruit-lcd-pabyggingskort-for-raspberry-pi-p87263)

![LCD display](https://farm2.staticflickr.com/1486/25712163793_69ef335a30_z.jpg)

## Build and test

lcd_app uses rebar3 as build tool.

### Compile

```sh
rebar3 compile
```

### Test

```sh
rebar3 shell
```

### Example

```sh
pi@rpi02:~/adafruit_lcd $ rebar3 shell
===> Verifying dependencies...
===> Compiling lcd_app
make: Entering directory '/home/pi/adafruit_lcd/c_src'
cc -o ../priv/i2c_drv.so -fpic -shared -Wall -Wformat i2c_drv.c
make: Leaving directory '/home/pi/adafruit_lcd/c_src'
Erlang/OTP 18 [erts-7.3] [source] [smp:4:4] [async-threads:0] [kernel-poll:false]

Eshell V7.3  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/v3.0/docs/releases)
===> Booted lcd_app

1> lcd:message("HELLO!").
ok
2>
```

## lcd methods

* clear/0,
* enable_display/1,
* show_cursor/1,
* message/1,
* home/0,
* set_cursor/2,
* blink/1,
* move_left/0,
* move_right/0,
* autoscroll/1
* backlight/1

## TODO

* Button support
* set_left_to_right
* set_right_to_left
