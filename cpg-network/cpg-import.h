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

typedef struct _CpgImport		CpgImport;
typedef struct _CpgImportClass		CpgImportClass;
typedef struct _CpgImportPrivate	CpgImportPrivate;

struct _CpgImport
{
	/*< private >*/
	CpgGroup parent;

	CpgImportPrivate *priv;
};

struct _CpgImportClass
{
	/*< private >*/
	CpgGroupClass parent_class;
};

GType cpg_import_get_type (void) G_GNUC_CONST;

CpgImport   *cpg_import_new               (CpgNetwork   *network,
                                           CpgGroup     *parent,
                                           const gchar  *id,
                                           const gchar  *filename,
                                           GError      **error);

gboolean     cpg_import_load              (CpgImport    *self,
                                           CpgNetwork   *network,
                                           CpgGroup     *parent,
                                           GError      **error);

gchar const *cpg_import_get_filename      (CpgImport    *self);

gboolean     cpg_import_get_auto_imported (CpgImport    *self);

G_END_DECLS

#endif /* __CPG_IMPORT_H__ */
