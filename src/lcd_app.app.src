{application, lcd_app,
 [{description, "Adafruid LCD 16x2, MCP23017, Raspberry Pi application"},
  {vsn, "1"},
  {modules, {lcd_app, lcd_sup, lcd, mcp, i2c}},
  {registered, [lcd, mcp, i2c]},
  {applications, [kernel, stdlib]},
  {mod, {lcd_app, []}},
  {env, [{i2c_device, "/dev/i2c-1"}]}
]}.