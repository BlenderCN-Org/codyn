#ifndef __CDN_CFILE_STREAM_H__
#define __CDN_CFILE_STREAM_H__

#include <gio/gio.h>
#include <stdio.h>

G_BEGIN_DECLS

#define CDN_TYPE_CFILE_STREAM			(cdn_cfile_stream_get_type ())
#define CDN_CFILE_STREAM(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_CFILE_STREAM, CdnCfileStream))
#define CDN_CFILE_STREAM_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_CFILE_STREAM, CdnCfileStream const))
#define CDN_CFILE_STREAM_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_CFILE_STREAM, CdnCfileStreamClass))
#define CDN_IS_CFILE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_CFILE_STREAM))
#define CDN_IS_CFILE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_CFILE_STREAM))
#define CDN_CFILE_STREAM_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_CFILE_STREAM, CdnCfileStreamClass))

typedef struct _CdnCfileStream		CdnCfileStream;
typedef struct _CdnCfileStreamClass	CdnCfileStreamClass;
typedef struct _CdnCfileStreamPrivate	CdnCfileStreamPrivate;

struct _CdnCfileStream
{
	/*< private >*/
	GOutputStream parent;

	CdnCfileStreamPrivate *priv;
};

struct _CdnCfileStreamClass
{
	/*< private >*/
	GOutputStreamClass parent_class;
};

GType            cdn_cfile_stream_get_type (void) G_GNUC_CONST;

GOutputStream   *cdn_cfile_stream_new       (FILE *cfile);

G_END_DECLS

#endif /* __CDN_CFILE_STREAM_H__ */
