#include "cdn-io-file.h"
#include "cdn-input-file.h"

void
cdn_io_register_types (GTypeModule *type_module)
{
	cdn_input_file_register (type_module);
}
