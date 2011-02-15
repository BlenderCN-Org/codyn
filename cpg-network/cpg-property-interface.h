#ifndef __CPG_PROPERTY_INTERFACE_H__
#define __CPG_PROPERTY_INTERFACE_H__

#include <glib-object.h>
#include <cpg-network/cpg-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_PROPERTY_INTERFACE		(cpg_property_interface_get_type ())
#define CPG_PROPERTY_INTERFACE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterface))
#define CPG_PROPERTY_INTERFACE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterface const))
#define CPG_PROPERTY_INTERFACE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterfaceClass))
#define CPG_IS_PROPERTY_INTERFACE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_PROPERTY_INTERFACE))
#define CPG_IS_PROPERTY_INTERFACE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_PROPERTY_INTERFACE))
#define CPG_PROPERTY_INTERFACE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterfaceClass))

#define CPG_PROPERTY_INTERFACE_ERROR (cpg_object_error_quark ())

/**
 * CpgPropertyInterfaceError:
 *
 * Enum used to indicate an error when interacting with the property interface
 *
 **/
typedef enum
{
	CPG_PROPERTY_INTERFACE_ERROR_EXISTS,
	CPG_PROPERTY_INTERFACE_ERROR_NOT_FOUND,
	CPG_PROPERTY_INTERFACE_ERROR_NUM_ERRORS
} CpgPropertyInterfaceError;

typedef struct _CpgPropertyInterface		CpgPropertyInterface;
typedef struct _CpgPropertyInterfaceClass	CpgPropertyInterfaceClass;
typedef struct _CpgPropertyInterfacePrivate	CpgPropertyInterfacePrivate;

struct _CpgPropertyInterface
{
	/*< private >*/
	GObject parent;

	CpgPropertyInterfacePrivate *priv;
};

struct _CpgPropertyInterfaceClass
{
	/*< private >*/
	GObjectClass parent_class;

	void (*added) (CpgPropertyInterface *iface, gchar const *name, CpgProperty *property);
	void (*removed) (CpgPropertyInterface *iface, gchar const *name, CpgProperty *property);
};

GQuark cpg_property_interface_error_quark (void);

GType cpg_property_interface_get_type (void) G_GNUC_CONST;
CpgPropertyInterface *cpg_property_interface_new (CpgObject *object);

CpgObject *cpg_property_interface_get_object (CpgPropertyInterface *iface);

gchar **cpg_property_interface_get_names (CpgPropertyInterface *iface);

CpgProperty *cpg_property_interface_lookup (CpgPropertyInterface *iface,
                                            gchar const          *name);

gboolean cpg_property_interface_add (CpgPropertyInterface  *iface,
                                     gchar const           *name,
                                     CpgProperty           *property,
                                     GError               **error);

gboolean cpg_property_interface_remove (CpgPropertyInterface  *iface,
                                        gchar const           *name,
                                        GError               **error);

G_END_DECLS

#endif /* __CPG_PROPERTY_INTERFACE_H__ */
