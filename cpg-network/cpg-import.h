#ifndef __CPG_IMPORT_H__
#define __CPG_IMPORT_H__

#include <cpg-network/cpg-network.h>

G_BEGIN_DECLS

#define CPG_TYPE_IMPORT			(cpg_import_get_type ())
#define CPG_IMPORT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_IMPORT, CpgImport))
#define CPG_IMPORT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_IMPORT, CpgImport const))
#define CPG_IMPORT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_IMPORT, CpgImportClass))
#define CPG_IS_IMPORT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_IMPORT))
#define CPG_IS_IMPORT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_IMPORT))
#define CPG_IMPORT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_IMPORT, CpgImportClass))

#define CPG_IMPORT_ERROR (cpg_import_error_quark ())

/**
 * CpgImportError:
 * @CPG_IMPORT_ERROR_REMOVE: cannot remove imported object
 *
 * Import error types.
 *
 */
typedef enum
{
	CPG_IMPORT_ERROR_REMOVE,
} CpgImportError;

typedef struct _CpgImport		CpgImport;
typedef struct _CpgImportClass		CpgImportClass;
typedef struct _CpgImportPrivate	CpgImportPrivate;

struct _CpgImport
{
	/*< private >*/
	CpgGroup parent;

	CpgImportPrivate *priv;
};

/**
 * CpgImportClass:
 *
 * The #CpgImport class
 *
 */
struct _CpgImportClass
{
	/*< private >*/
	CpgGroupClass parent_class;
};

GType cpg_import_get_type (void) G_GNUC_CONST;

GQuark cpg_import_error_quark (void);

CpgImport   *cpg_import_new                 (CpgNetwork   *network,
                                             CpgGroup     *parent,
                                             const gchar  *id,
                                             GFile        *file,
                                             GError      **error);

CpgImport   *cpg_import_new_from_path       (CpgNetwork   *network,
                                             CpgGroup     *parent,
                                             const gchar  *id,
                                             const gchar  *path,
                                             GError      **error);

gboolean     cpg_import_load                (CpgImport    *self,
                                             CpgNetwork   *network,
                                             CpgGroup     *parent,
                                             GError      **error);

GFile       *cpg_import_get_file            (CpgImport    *self);
gchar       *cpg_import_get_path            (CpgImport    *self);

gboolean     cpg_import_imports_object      (CpgImport    *self,
                                             CpgObject    *object);

G_CONST_RETURN gchar * G_CONST_RETURN *
             cpg_import_get_search_path     ();

void         cpg_import_set_search_path     (gchar **path);

void         cpg_import_append_search_path  (const gchar  *path);

void         cpg_import_prepend_search_path (const gchar  *path);


G_END_DECLS

#endif /* __CPG_IMPORT_H__ */
