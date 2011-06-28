#ifndef __CPG_IMPORT_ALIAS_H__
#define __CPG_IMPORT_ALIAS_H__

#include <cpg-network/cpg-import.h>

G_BEGIN_DECLS

#define CPG_TYPE_IMPORT_ALIAS            (cpg_import_alias_get_type ())
#define CPG_IMPORT_ALIAS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_IMPORT_ALIAS, CpgImportAlias))
#define CPG_IMPORT_ALIAS_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_IMPORT_ALIAS, CpgImportAlias const))
#define CPG_IMPORT_ALIAS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_IMPORT_ALIAS, CpgImportAliasClass))
#define CPG_IS_IMPORT_ALIAS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_IMPORT_ALIAS))
#define CPG_IS_IMPORT_ALIAS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_IMPORT_ALIAS))
#define CPG_IMPORT_ALIAS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_IMPORT_ALIAS, CpgImportAliasClass))

typedef struct _CpgImportAlias        CpgImportAlias;
typedef struct _CpgImportAliasClass   CpgImportAliasClass;
typedef struct _CpgImportAliasPrivate CpgImportAliasPrivate;

struct _CpgImportAlias
{
	/*< private >*/
	CpgImport parent;

	CpgImportAliasPrivate *priv;
};

/**
 * CpgImportAliasClass:
 *
 * The #CpgImportAlias class
 *
 */
struct _CpgImportAliasClass
{
	/*< private >*/
	CpgImportClass parent_class;
};

GType cpg_import_alias_get_type (void) G_GNUC_CONST;

CpgImportAlias *cpg_import_alias_new (CpgImport *source);

G_END_DECLS

#endif /* __CPG_IMPORT_ALIAS_H__ */
