#ifndef __CPG_OPERATOR_H__
#define __CPG_OPERATOR_H__

#include <glib-object.h>
#include <cpg-network/cpg-stack.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR		(cpg_operator_get_type ())
#define CPG_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR, CpgOperator))
#define CPG_IS_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR))
#define CPG_OPERATOR_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_OPERATOR, CpgOperatorInterface))

typedef struct _CpgOperator          CpgOperator;
typedef struct _CpgOperatorInterface CpgOperatorInterface;

typedef struct _CpgOperatorData      CpgOperatorData;

struct _CpgOperatorData
{
	GSList *expressions;
};

struct _CpgOperatorInterface
{
	/*< private >*/
	GTypeInterface parent;

	/*< public >*/
	void             (*execute)     (CpgOperator     *op,
	                                 CpgOperatorData *data,
	                                 CpgStack        *stack);

	gchar           *(*get_name)    (CpgOperator     *op);

	CpgOperatorData *(*create_data) (CpgOperator     *op,
	                                 GSList          *expressions);

	void             (*free_data)   (CpgOperator     *op,
	                                 CpgOperatorData *data);

	gint             (*get_num_arguments) (CpgOperator *op);

	void             (*reset_cache) (CpgOperator     *op,
	                                 CpgOperatorData *data);

	void             (*reset_variadic) (CpgOperator     *op,
	                                    CpgOperatorData *data);

	GSList const    *(*get_expressions) (CpgOperator     *op,
	                                     CpgOperatorData *data);
};

#define cpg_operator_data_new(Type, expressions) ((Type *)cpg_operator_data_init ((CpgOperatorData *)g_slice_new0 (Type), expressions))
#define cpg_operator_data_free(Type, data) (g_slice_free (Type, data))

CpgOperatorData     *cpg_operator_data_init                   (CpgOperatorData *data,
                                                               GSList          *expressions);

void                 cpg_operator_data_destroy                (CpgOperatorData *data);

GType                cpg_operator_get_type                    (void) G_GNUC_CONST;

CpgOperatorData     *cpg_operator_create_data                 (CpgOperator     *op,
                                                               GSList          *expressions);

void                 cpg_operator_free_data                   (CpgOperator     *op,
                                                               CpgOperatorData *data);

void                 cpg_operator_execute                     (CpgOperator     *op,
                                                               CpgOperatorData *data,
                                                               CpgStack        *stack);

void                 cpg_operator_reset_cache                 (CpgOperator     *op,
                                                               CpgOperatorData *data);
void                 cpg_operator_reset_variadic              (CpgOperator     *op,
                                                               CpgOperatorData *data);

gchar               *cpg_operator_get_name                    (CpgOperator     *op);
gint                 cpg_operator_get_num_arguments           (CpgOperator     *op);

GSList const        *cpg_operator_get_expressions             (CpgOperator     *op,
                                                               CpgOperatorData *data);

G_END_DECLS

#endif /* __CPG_OPERATOR_H__ */
