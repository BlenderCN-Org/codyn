#ifndef __CPG_NETWORK_SERIALIZER_H__
#define __CPG_NETWORK_SERIALIZER_H__

#include <gio/gio.h>
#include <cpg-network/cpg-network.h>

G_BEGIN_DECLS

#define CPG_TYPE_NETWORK_SERIALIZER		(cpg_network_serializer_get_type ())
#define CPG_NETWORK_SERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializer))
#define CPG_NETWORK_SERIALIZER_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializer const))
#define CPG_NETWORK_SERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializerClass))
#define CPG_IS_NETWORK_SERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_NETWORK_SERIALIZER))
#define CPG_IS_NETWORK_SERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_NETWORK_SERIALIZER))
#define CPG_NETWORK_SERIALIZER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializerClass))

typedef struct _CpgNetworkSerializer		CpgNetworkSerializer;
typedef struct _CpgNetworkSerializerClass	CpgNetworkSerializerClass;
typedef struct _CpgNetworkSerializerPrivate	CpgNetworkSerializerPrivate;

struct _CpgNetworkSerializer
{
	/*< private >*/
	GObject parent;

	CpgNetworkSerializerPrivate *priv;
};

struct _CpgNetworkSerializerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_network_serializer_get_type (void) G_GNUC_CONST;

CpgNetworkSerializer *cpg_network_serializer_new (CpgNetwork *network,
                                                  CpgGroup   *root);

gboolean cpg_network_serializer_serialize (CpgNetworkSerializer  *serializer,
                                           GOutputStream         *stream,
                                           GError               **error);

G_END_DECLS

#endif /* __CPG_NETWORK_SERIALIZER_H__ */
