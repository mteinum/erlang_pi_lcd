# Adafruit 16x2 LCD for Rasperry Pi

Erlang/OTP Application for the 16x2 LCD with MCP23017 on Raspberry Pi.

This is a port of the Python source code to Erlang.

* MCP code inspired by [MCP230xx](https://github.com/adafruit/Adafruit_Python_GPIO/blob/master/Adafruit_GPIO/MCP230xx.py)
* LCD code inspired by [Adafruit_CharLCD](https://github.com/adafruit/Adafruit_Python_CharLCD/blob/master/Adafruit_CharLCD/Adafruit_CharLCD.py)

![LCD display](https://farm2.staticflickr.com/1486/25712163793_69ef335a30_z.jpg)

## Build

The portdriver must be compiled

```sh
pi@rpi02:~/adafruit_lcd/c_src $ make
cc -o ../ebin/i2c_drv.so -fpic -shared -Wall -Wformat i2c_drv.c
```

Then compile the erlang source code:

```sh
pi@rpi02:~/adafruit_lcd $ erl -make
Recompile: src/mcp
Recompile: src/lcd_sup
Recompile: src/lcd_app
Recompile: src/lcd
Recompile: src/i2c
```

## Usage

```erlang
application:start(lcd_app).

lcd:message("#ERLANG ROCKS").
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

## TODO

* Button support
* message/1 multiline support
* set_left_to_right
* set_right_to_left
* set_backlight
* create_char
