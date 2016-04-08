# erlang_pi_lcd

Adafruit 16x2 LCD for Rasperry Pi using MCP23017. Erlang portdriver.

MCP code inspired by https://github.com/adafruit/Adafruit_Python_GPIO/blob/master/Adafruit_GPIO/MCP230xx.py

LCD code inspired by https://github.com/adafruit/Adafruit_Python_CharLCD/blob/master/Adafruit_CharLCD/Adafruit_CharLCD.py

![LCD display](https://farm2.staticflickr.com/1486/25712163793_69ef335a30_z.jpg)

Erlang/OTP application for communicating with the Adafruid 16x2 LCD display.

## Usage

```erlang
application:start(pi_lcd_app).

pi_lcd:message("#ERLANG ROCKS").
```

## TODO

* Button support
* Multiline
* set_cursor
* blink
* move_left
* move_right
* set_left_to_right
* set_right_to_left
* autoscroll
* set_backlight
* create_char
