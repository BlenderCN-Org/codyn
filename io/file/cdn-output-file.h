#ifndef __CDN_OUTPUT_FILE_H__
#define __CDN_OUTPUT_FILE_H__

#include <codyn/cdn-node.h>

G_BEGIN_DECLS

#define CDN_TYPE_OUTPUT_FILE		(cdn_output_file_get_type ())
#define CDN_OUTPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OUTPUT_FILE, CdnOutputFile))
#define CDN_OUTPUT_FILE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OUTPUT_FILE, CdnOutputFile const))
#define CDN_OUTPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_OUTPUT_FILE, CdnOutputFileClass))
#define CDN_IS_OUTPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_OUTPUT_FILE))
#define CDN_IS_OUTPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_OUTPUT_FILE))
#define CDN_OUTPUT_FILE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_OUTPUT_FILE, CdnOutputFileClass))

typedef struct _CdnOutputFile		CdnOutputFile;
typedef struct _CdnOutputFileClass	CdnOutputFileClass;
typedef struct _CdnOutputFilePrivate	CdnOutputFilePrivate;

struct _CdnOutputFile
{
	/*< private >*/
	CdnNode parent;

	CdnOutputFilePrivate *priv;
};

struct _CdnOutputFileClass
{
	/*< private >*/
	CdnNodeClass parent_class;
};

GType cdn_output_file_get_type (void) G_GNUC_CONST;
void  cdn_output_file_register (GTypeModule *type_module);

G_END_DECLS

#endif /* __CDN_OUTPUT_FILE_H__ */
