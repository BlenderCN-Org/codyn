#ifndef __CDN_NETWORK_THREAD_H__
#define __CDN_NETWORK_THREAD_H__

#include <glib-object.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define CDN_TYPE_NETWORK_THREAD			(cdn_network_thread_get_type ())
#define CDN_NETWORK_THREAD(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_THREAD, CdnNetworkThread))
#define CDN_NETWORK_THREAD_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_THREAD, CdnNetworkThread const))
#define CDN_NETWORK_THREAD_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_NETWORK_THREAD, CdnNetworkThreadClass))
#define CDN_IS_NETWORK_THREAD(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_NETWORK_THREAD))
#define CDN_IS_NETWORK_THREAD_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_NETWORK_THREAD))
#define CDN_NETWORK_THREAD_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_NETWORK_THREAD, CdnNetworkThreadClass))

typedef struct _CdnNetworkThread	CdnNetworkThread;
typedef struct _CdnNetworkThreadClass	CdnNetworkThreadClass;
typedef struct _CdnNetworkThreadPrivate	CdnNetworkThreadPrivate;

struct _CdnNetworkThread
{
	GObject parent;

	CdnNetworkThreadPrivate *priv;
};

struct _CdnNetworkThreadClass
{
	GObjectClass parent_class;
};

GType cdn_network_thread_get_type (void) G_GNUC_CONST;
void _cdn_network_thread_register (GTypeModule *type_module);

CdnNetworkThread *cdn_network_thread_get_default ();

void cdn_network_thread_register (CdnNetworkThread  *self,
                                  GSocket           *socket,
                                  GSocketSourceFunc  callback,
                                  gpointer           user_data,
                                  GDestroyNotify     destroy_notify);

void cdn_network_thread_unregister (CdnNetworkThread *self,
                                    GSocket          *socket);

G_END_DECLS

#endif /* __CDN_NETWORK_THREAD_H__ */
