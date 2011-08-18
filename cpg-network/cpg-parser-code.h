#ifndef __CPG_PARSER_CODE_H__
#define __CPG_PARSER_CODE_H__

#include <glib-object.h>
#include <cpg-network/cpg-embedded-context.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_PARSER_CODE			(cpg_parser_code_get_type ())
#define CPG_PARSER_CODE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PARSER_CODE, CpgParserCode))
#define CPG_PARSER_CODE_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PARSER_CODE, CpgParserCode const))
#define CPG_PARSER_CODE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_PARSER_CODE, CpgParserCodeClass))
#define CPG_IS_PARSER_CODE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_PARSER_CODE))
#define CPG_IS_PARSER_CODE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_PARSER_CODE))
#define CPG_PARSER_CODE_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_PARSER_CODE, CpgParserCodeClass))

typedef struct _CpgParserCode		CpgParserCode;
typedef struct _CpgParserCodeClass	CpgParserCodeClass;
typedef struct _CpgParserCodePrivate	CpgParserCodePrivate;

typedef enum
{
	CPG_PARSER_CODE_EVENT_NONE,
	CPG_PARSER_CODE_EVENT_BEFORE_APPLY,
	CPG_PARSER_CODE_EVENT_AFTER_APPLY,
	CPG_PARSER_CODE_EVENT_BEFORE_UNAPPLY,
	CPG_PARSER_CODE_EVENT_AFTER_UNAPPLY
} CpgParserCodeEvent;

CPG_FORWARD_DECL (CpgObject);

struct _CpgParserCode
{
	/*< private >*/
	GObject parent;

	CpgParserCodePrivate *priv;
};

struct _CpgParserCodeClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType               cpg_parser_code_get_type     (void) G_GNUC_CONST;

CpgParserCode      *cpg_parser_code_new          (CpgEmbeddedContext  *closure,
                                                   gchar const        *code,
                                                   CpgParserCodeEvent  event);

gboolean            cpg_parser_code_run          (CpgParserCode     *applied,
                                                  CPG_FORWARD_DECL (CpgObject) *object,
                                                  GError            **error);

CpgEmbeddedContext *cpg_parser_code_get_closure  (CpgParserCode     *applied);
gchar const        *cpg_parser_code_get_code     (CpgParserCode     *applied);
CpgParserCodeEvent  cpg_parser_code_get_event    (CpgParserCode     *applied);

G_END_DECLS

#endif /* __CPG_PARSER_CODE_H__ */
