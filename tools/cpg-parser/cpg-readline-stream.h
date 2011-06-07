#ifndef __CPG_READLINE_STREAM_H__
#define __CPG_READLINE_STREAM_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define CPG_TYPE_READLINE_STREAM		(cpg_readline_stream_get_type ())
#define CPG_READLINE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_READLINE_STREAM, CpgReadlineStream))
#define CPG_READLINE_STREAM_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_READLINE_STREAM, CpgReadlineStream const))
#define CPG_READLINE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_READLINE_STREAM, CpgReadlineStreamClass))
#define CPG_IS_READLINE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_READLINE_STREAM))
#define CPG_IS_READLINE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_READLINE_STREAM))
#define CPG_READLINE_STREAM_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_READLINE_STREAM, CpgReadlineStreamClass))

typedef struct _CpgReadlineStream		CpgReadlineStream;
typedef struct _CpgReadlineStreamClass		CpgReadlineStreamClass;
typedef struct _CpgReadlineStreamPrivate	CpgReadlineStreamPrivate;

struct _CpgReadlineStream
{
	/*< private >*/
	GInputStream parent;

	CpgReadlineStreamPrivate *priv;
};

struct _CpgReadlineStreamClass
{
	/*< private >*/
	GInputStreamClass parent_class;
};

GType cpg_readline_stream_get_type (void) G_GNUC_CONST;

GInputStream *cpg_readline_stream_new (gchar const *prompt);

G_END_DECLS

#endif /* __CPG_READLINE_STREAM_H__ */
