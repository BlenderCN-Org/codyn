#ifndef __CDN_CLIENT_H__
#define __CDN_CLIENT_H__

#include <glib-object.h>
#include <gio/gio.h>
#include <codyn/cdn-node.h>
#include <codyn/cdn-io.h>
#include <codyn/cdn-enum-types.h>

G_BEGIN_DECLS

#define CDN_TYPE_CLIENT			(cdn_client_get_type ())
#define CDN_CLIENT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_CLIENT, CdnClient))
#define CDN_CLIENT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_CLIENT, CdnClient const))
#define CDN_CLIENT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_CLIENT, CdnClientClass))
#define CDN_IS_CLIENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_CLIENT))
#define CDN_IS_CLIENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_CLIENT))
#define CDN_CLIENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_CLIENT, CdnClientClass))

typedef struct _CdnClient		CdnClient;
typedef struct _CdnClientClass		CdnClientClass;
typedef struct _CdnClientPrivate	CdnClientPrivate;

struct _CdnClient
{
	GObject parent;

	CdnClientPrivate *priv;
};

struct _CdnClientClass
{
	GObjectClass parent_class;
};

GType      cdn_client_get_type   (void) G_GNUC_CONST;

CdnClient *cdn_client_new        (CdnNode        *node,
                                  GSocket        *socket,
                                  GSocketAddress *addr,
                                  CdnIoMode       io_mode,
                                  gdouble         throttle);

void       cdn_client_initialize (CdnClient      *client);
void       cdn_client_update     (CdnClient      *client);
void       cdn_client_close      (CdnClient      *client);

G_END_DECLS

#endif /* __CDN_CLIENT_H__ */
