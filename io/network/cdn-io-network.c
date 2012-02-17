#include "cdn-io-network.h"
#include "cdn-io-network-enum-types.h"
#include "cdn-io-network-client.h"
#include "cdn-io-network-server.h"
#include "cdn-network-thread.h"

void
cdn_io_register_types (GTypeModule *type_module)
{
	cdn_network_protocol_register (type_module);

	_cdn_network_thread_register (type_module);

	cdn_io_network_client_register (type_module);
	cdn_io_network_server_register (type_module);
}
