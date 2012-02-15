#ifndef __CDN_INPUT_FILE_H__
#define __CDN_INPUT_FILE_H__

#include <codyn/cdn-io.h>
#include <codyn/cdn-function.h>

G_BEGIN_DECLS

#define CDN_TYPE_INPUT_FILE		(cdn_input_file_get_type ())
#define CDN_INPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_FILE, CdnInputFile))
#define CDN_INPUT_FILE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_FILE, CdnInputFile const))
#define CDN_INPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INPUT_FILE, CdnInputFileClass))
#define CDN_IS_INPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INPUT_FILE))
#define CDN_IS_INPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INPUT_FILE))
#define CDN_INPUT_FILE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INPUT_FILE, CdnInputFileClass))

typedef struct _CdnInputFile		CdnInputFile;
typedef struct _CdnInputFileClass	CdnInputFileClass;
typedef struct _CdnInputFilePrivate	CdnInputFilePrivate;

struct _CdnInputFile
{
	/*< private >*/
	CdnFunction parent;

	CdnInputFilePrivate *priv;
};

struct _CdnInputFileClass
{
	/*< private >*/
	CdnFunctionClass parent_class;
};

GType cdn_input_file_get_type (void) G_GNUC_CONST;
void  cdn_input_file_register (GTypeModule *type_module);

G_END_DECLS

#endif /* __CDN_INPUT_FILE_H__ */
