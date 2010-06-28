#ifndef __CPG_NETWORK_DESERIALIZER_H__
#define __CPG_NETWORK_DESERIALIZER_H__

#include <gio/gio.h>
#include <cpg-network/cpg-network.h>

G_BEGIN_DECLS

#define CPG_TYPE_NETWORK_DESERIALIZER			(cpg_network_deserializer_get_type ())
#define CPG_NETWORK_DESERIALIZER(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializer))
#define CPG_NETWORK_DESERIALIZER_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializer const))
#define CPG_NETWORK_DESERIALIZER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializerClass))
#define CPG_IS_NETWORK_DESERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_NETWORK_DESERIALIZER))
#define CPG_IS_NETWORK_DESERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_NETWORK_DESERIALIZER))
#define CPG_NETWORK_DESERIALIZER_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializerClass))

typedef struct _CpgNetworkDeserializer		CpgNetworkDeserializer;
typedef struct _CpgNetworkDeserializerClass	CpgNetworkDeserializerClass;
typedef struct _CpgNetworkDeserializerPrivate	CpgNetworkDeserializerPrivate;

struct _CpgNetworkDeserializer
{
	/*< private >*/
	GObject parent;

	CpgNetworkDeserializerPrivate *priv;
};

struct _CpgNetworkDeserializerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_network_deserializer_get_type (void) G_GNUC_CONST;
CpgNetworkDeserializer *cpg_network_deserializer_new (CpgNetwork *network,
                                                      CpgGroup   *root);

gboolean cpg_network_deserializer_deserialize (CpgNetworkDeserializer  *deserializer,
                                               GInputStream            *stream,
                                               GError                 **error);

gboolean cpg_network_deserializer_deserialize_file (CpgNetworkDeserializer  *deserializer,
                                                    GFile                   *file,
                                                    GError                 **error);

gboolean cpg_network_deserializer_deserialize_path (CpgNetworkDeserializer  *deserializer,
                                                    const gchar             *path,
                                                    GError                 **error);

G_END_DECLS

#endif /* __CPG_NETWORK_DESERIALIZER_H__ */
