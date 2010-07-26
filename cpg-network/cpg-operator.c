#include "cpg-operator.h"
#include "cpg-expression.h"

G_DEFINE_INTERFACE (CpgOperator, cpg_operator, G_TYPE_OBJECT);

/* Default implementation */
static gchar *
cpg_operator_get_name_default (CpgOperator *op)
{
	g_return_val_if_reached (NULL);
}

static CpgOperatorData *
cpg_operator_create_data_default (CpgOperator *op,
                                  GSList      *expressions)
{
	return cpg_operator_data_new (CpgOperatorData, expressions);
}

static void
cpg_operator_free_data_default (CpgOperator     *op,
                                CpgOperatorData *data)
{
	cpg_operator_data_destroy (data);
	g_slice_free (CpgOperatorData, data);
}

static void
cpg_operator_execute_default (CpgOperator     *op,
                              CpgOperatorData *data,
                              CpgStack        *stack)
{
	cpg_stack_push (stack, 0);
}

static void
cpg_operator_reset_cache_default (CpgOperator     *op,
                                  CpgOperatorData *data)
{
	g_slist_foreach (data->expressions,
	                 (GFunc)cpg_expression_reset_cache,
	                 NULL);
}

static void
cpg_operator_reset_variadic_default (CpgOperator     *op,
                                     CpgOperatorData *data)
{
	g_slist_foreach (data->expressions,
	                 (GFunc)cpg_expression_reset_variadic,
	                 NULL);
}

static gint
cpg_operator_get_num_arguments_default (CpgOperator *op)
{
	return 1;
}

static void
cpg_operator_default_init (CpgOperatorInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->create_data = cpg_operator_create_data_default;
	iface->free_data = cpg_operator_free_data_default;

	iface->execute = cpg_operator_execute_default;
	iface->get_name = cpg_operator_get_name_default;
	iface->get_num_arguments = cpg_operator_get_num_arguments_default;
	iface->reset_cache = cpg_operator_reset_cache_default;
	iface->reset_variadic = cpg_operator_reset_variadic_default;

	if (!initialized)
	{
		initialized = TRUE;
	}
}

CpgOperatorData *
cpg_operator_data_init (CpgOperatorData *data,
                        GSList          *expressions)
{
	data->expressions = g_slist_copy (expressions);
	g_slist_foreach (data->expressions, (GFunc)g_object_ref, NULL);

	return data;
}

void
cpg_operator_data_destroy (CpgOperatorData *data)
{
	g_slist_foreach (data->expressions, (GFunc)g_object_unref, NULL);
	g_slist_free (data->expressions);
}

CpgOperatorData *
cpg_operator_create_data (CpgOperator *op,
                          GSList      *expressions)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_INTERFACE (op)->create_data (op,
	                                                     expressions);
}

void
cpg_operator_free_data (CpgOperator     *op,
                        CpgOperatorData *data)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	if (data)
	{
		return CPG_OPERATOR_GET_INTERFACE (op)->free_data (op, data);
	}
}

void
cpg_operator_execute (CpgOperator     *op,
                      CpgOperatorData *data,
                      CpgStack        *stack)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	return CPG_OPERATOR_GET_INTERFACE (op)->execute (op, data, stack);
}

gchar *
cpg_operator_get_name (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_INTERFACE (op)->get_name (op);
}

gint
cpg_operator_get_num_arguments (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), 0);

	return CPG_OPERATOR_GET_INTERFACE (op)->get_num_arguments (op);
}

void
cpg_operator_reset_cache (CpgOperator     *op,
                          CpgOperatorData *data)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	return CPG_OPERATOR_GET_INTERFACE (op)->reset_cache (op, data);
}

void
cpg_operator_reset_variadic (CpgOperator     *op,
                             CpgOperatorData *data)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	return CPG_OPERATOR_GET_INTERFACE (op)->reset_variadic (op, data);
}
