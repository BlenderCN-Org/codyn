#include "cdn-network-xml.h"
#include <libxml/tree.h>

void
cdn_network_xml_init ()
{
	if (!xmlFree)
	{
		xmlMemGet(&xmlFree, &xmlMalloc, &xmlRealloc, NULL);
	}
}
