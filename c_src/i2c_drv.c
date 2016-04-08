/*
    Minimalistic I2C port driver for integration with the
	MCP23017 chip in use with the Adafruit LCD 16x2 board.

    morten.teinum@gmail.com
*/

#include <stdio.h>
#include <linux/i2c-dev.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <string.h>

#include "erl_driver.h"

// commands for (*control)

#define CMD_WRITE_BLOCK_DATA 3
#define CMD_OPEN             4

typedef struct {
    ErlDrvPort port;
    int fd;
    int verbose;
} i2c_drv_portdata;

static ErlDrvData i2c_drv_start(ErlDrvPort port, char *buff)
{
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    i2c_drv_portdata* d = (i2c_drv_portdata*)driver_alloc(sizeof(i2c_drv_portdata));
    d->port = port;
    d->fd = 0;
    d->verbose = 0;

    return (ErlDrvData)d;
}

static void i2c_drv_stop(ErlDrvData handle)
{
    fprintf(stderr, "stop\r\n");

    i2c_drv_portdata* d = (i2c_drv_portdata*)handle;

    if (d->fd)
        close(d->fd);

    driver_free((char*)handle);
}

static void i2c_drv_ctl_write_block_data(i2c_drv_portdata* d, char* buf0, ErlDrvSizeT len) {
    
    __s32 result;
    union i2c_smbus_data data;

    data.block[0] = 2;
    data.block[1] = buf0[1];
    data.block[2] = buf0[2];

    result = i2c_smbus_access(
            d->fd,
            I2C_SMBUS_WRITE,
            (__u8)buf0[0],
            I2C_SMBUS_I2C_BLOCK_BROKEN,
            &data);

    if (result || d->verbose)
            fprintf(stderr, "i2c_smbus_write_block_data reg=0x%02X: [%i, %i] result=%i\r\n",
                buf0[0], buf0[1], buf0[2], result);
}

static void i2c_drv_ctl_open(i2c_drv_portdata* d, char* buf0, ErlDrvSizeT len){
    int ret;
    char driver_path[0x1000];
    int addr = 0x20;
    memset(driver_path, 0, sizeof(driver_path));
    memcpy(driver_path, buf0, len);

    d->fd = open(driver_path, O_RDWR, 0);

    if (d->verbose)
        fprintf(stderr, "open %i\r\n", d->fd);
    
    if (d->fd) {
        ret = ioctl(d->fd, I2C_SLAVE, addr);

        if (d->verbose)
            fprintf(stderr, "ioctl %i\r\n", ret);
    }
}

static ErlDrvSSizeT i2c_drv_ctl(ErlDrvData handle, 
                 unsigned int cmd, char* buf0, ErlDrvSizeT len,
                 char** rbuf, ErlDrvSizeT rsize)
{
    i2c_drv_portdata* d = (i2c_drv_portdata*)handle;

    if (cmd == CMD_WRITE_BLOCK_DATA && len == 3) {
        i2c_drv_ctl_write_block_data(d, buf0, len);
    }
    else if (cmd == CMD_OPEN) {
        i2c_drv_ctl_open(d, buf0, len);
    }

    return 0;
}

ErlDrvEntry i2c_drv_portdriver_entry = {
    NULL,			/* F_PTR init, called when driver is loaded */
    i2c_drv_start,		/* L_PTR start, called when port is opened */
    i2c_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,		/* F_PTR output, called when erlang has sent */
    NULL,			/* F_PTR ready_input, called when input descriptor ready */
    NULL,			/* F_PTR ready_output, called when output descriptor ready */
    "i2c_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,                       /* void *handle, Reserved by VM */
    i2c_drv_ctl,			/* F_PTR control, port_command callback */
    NULL,			/* F_PTR timeout, reserved */
    NULL,			/* F_PTR outputv, reserved */
    NULL,                       /* F_PTR ready_async, only for async drivers */
    NULL,                       /* F_PTR flush, called when port is about 
				   to be closed, but there is data in driver 
				   queue */
    NULL,                       /* F_PTR call, much like control, sync call
				   to driver */
    NULL,                       /* F_PTR event, called when an event selected 
				   by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
				   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				       set to this value */
    0,                          /* int driver_flags, see documentation */
    NULL,                       /* void *handle2, reserved for VM use */
    NULL,                       /* F_PTR process_exit, called when a 
				   monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an 
				   event object */
};

DRIVER_INIT(i2c_drv) /* must match name in driver_entry */
{
    return &i2c_drv_portdriver_entry;
}
