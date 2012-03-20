#ifndef __CDN_IO_METHOD_H__
#define __CDN_IO_METHOD_H__

#include <glib-object.h>
#include <gmodule.h>
#include <codyn/cdn-io.h>

G_BEGIN_DECLS

#define CDN_TYPE_IO_METHOD			(cdn_io_method_get_type ())
#define CDN_IO_METHOD(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_METHOD, CdnIoMethod))
#define CDN_IO_METHOD_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_METHOD, CdnIoMethod const))
#define CDN_IO_METHOD_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_IO_METHOD, CdnIoMethodClass))
#define CDN_IS_IO_METHOD(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IO_METHOD))
#define CDN_IS_IO_METHOD_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_IO_METHOD))
#define CDN_IO_METHOD_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_IO_METHOD, CdnIoMethodClass))

typedef void (*CdnIoMethodRegisterTypesFunc) (GTypeModule *module);

typedef struct _CdnIoMethod		CdnIoMethod;
typedef struct _CdnIoMethodClass	CdnIoMethodClass;
typedef struct _CdnIoMethodPrivate	CdnIoMethodPrivate;

struct _CdnIoMethod
{
	/*< private >*/
	GTypeModule parent;

	CdnIoMethodPrivate *priv;
};

struct _CdnIoMethodClass
{
	/*< private >*/
	GTypeModuleClass parent_class;
};

GType           cdn_io_method_get_type   (void) G_GNUC_CONST;
CdnIoMethod    *cdn_io_method_new        (gchar const *path);

GType           cdn_io_method_find       (gchar const *name,
                                          CdnIoMode    mode);

void            cdn_io_method_initialize (void);

G_END_DECLS

#endif /* __CDN_IO_METHOD_H__ */
