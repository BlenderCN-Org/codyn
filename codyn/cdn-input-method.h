#ifndef __CDN_INPUT_METHOD_H__
#define __CDN_INPUT_METHOD_H__

#include <glib-object.h>
#include <gmodule.h>

G_BEGIN_DECLS

#define CDN_TYPE_INPUT_METHOD			(cdn_input_method_get_type ())
#define CDN_INPUT_METHOD(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_METHOD, CdnInputMethod))
#define CDN_INPUT_METHOD_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_METHOD, CdnInputMethod const))
#define CDN_INPUT_METHOD_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INPUT_METHOD, CdnInputMethodClass))
#define CDN_IS_INPUT_METHOD(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INPUT_METHOD))
#define CDN_IS_INPUT_METHOD_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INPUT_METHOD))
#define CDN_INPUT_METHOD_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INPUT_METHOD, CdnInputMethodClass))

typedef void (*CdnInputMethodRegisterTypesFunc) (GTypeModule *module);

typedef struct _CdnInputMethod		CdnInputMethod;
typedef struct _CdnInputMethodClass	CdnInputMethodClass;
typedef struct _CdnInputMethodPrivate	CdnInputMethodPrivate;

struct _CdnInputMethod
{
	/*< private >*/
	GTypeModule parent;

	CdnInputMethodPrivate *priv;
};

struct _CdnInputMethodClass
{
	/*< private >*/
	GTypeModuleClass parent_class;
};

GType           cdn_input_method_get_type   (void) G_GNUC_CONST;
CdnInputMethod *cdn_input_method_new        (gchar const *path);

GType           cdn_input_method_find       (gchar const *name);

void            cdn_input_method_initialize (void);

G_END_DECLS

#endif /* __CDN_INPUT_METHOD_H__ */
