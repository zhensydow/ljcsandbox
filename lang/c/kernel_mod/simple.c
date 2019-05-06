#include <linux/module.h>
#include <linux/moduleparam.h>

MODULE_LICENSE("Dual BSD/GPL");
MODULE_AUTHOR("Luis Cabellos");

static int simple_init(void)
{
    printk(KERN_ALERT "hello...\n");
    return 0;
}

static void simple_cleanup(void)
{
    printk(KERN_WARNING "...bye\n");
}

module_init(simple_init);
module_exit(simple_cleanup);
