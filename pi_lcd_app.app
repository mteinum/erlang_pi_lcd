{application, pi_lcd_app,
 [{description, "Adafruid LCD 16x2 integration"},
  {vsn, "1"},
  {modules, {pi_lcd_app, pi_lcd_sup, pi_lcd, mcp, i2c}},
  {registered, [pi_lcd, mcp, i2c]},
  {applications, [kernel, stdlib]},
  {mod, {pi_lcd_app, []}}
]}.