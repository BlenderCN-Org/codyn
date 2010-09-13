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
cpg_operator_create_data_default (CpgOperator  *op,
                                  GSList const *expressions)
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

static GSList const *
cpg_operator_get_expressions_default (CpgOperator     *op,
                                      CpgOperatorData *data)
{
	return data->expressions;
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
	iface->get_expressions = cpg_operator_get_expressions_default;

	if (!initialized)
	{
		initialized = TRUE;
	}
}

/**
 * cpg_operator_data_init:
 * @data: A #CpgOperatorData
 * @expressions: (element-type CpgExpression) (transfer none): A #GSList of #CpgExpression
 *
 * Initialize the operator data with a set of expressions. When 'inheriting'
 * from #CpgOperatorData, make sure to call #cpg_operator_data_init from
 * the overloaded #cpg_operator_create_data.
 *
 * Returns: (transfer none): @data for convenience
 *
 **/
CpgOperatorData *
cpg_operator_data_init (CpgOperatorData *data,
                        GSList const    *expressions)
{
	data->expressions = g_slist_copy ((GSList *)expressions);
	g_slist_foreach (data->expressions, (GFunc)g_object_ref, NULL);

	return data;
}

/**
 * cpg_operator_data_destroy:
 * @data: A #CpgOperatorData
 *
 * Destroy the operator data. This frees the expressions stored in the
 * #CpgOperatorData (but does not free the #CpgOperatorData itself, see
 * also #cpg_operator_data_free). This function should be used when
 * an operator implementation inherits its own #CpgOperatorData.
 *
 **/
void
cpg_operator_data_destroy (CpgOperatorData *data)
{
	g_slist_foreach (data->expressions, (GFunc)g_object_unref, NULL);
	g_slist_free (data->expressions);
}

/**
 * cpg_operator_create_data:
 * @op: A #CpgOperator
 * @expressions: (element-type CpgExpression) (transfer none): A #GSList of #CpgExpression
 *
 * Create a new data instance for the operator, given a set of expressions.
 * Each instance of the operator has its own operator data associated that
 * will be provided to #cpg_operator_evaluate.
 *
 * Returns: (transfer full): A #CpgOperatorData
 *
 **/
CpgOperatorData *
cpg_operator_create_data (CpgOperator  *op,
                          GSList const *expressions)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_INTERFACE (op)->create_data (op,
	                                                     expressions);
}

/**
 * cpg_operator_free_data:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 *
 * Free the operator data. This default implementation will first call
 * #cpg_operator_data_destroy, after which the operator data slice is freed.
 * If you have a custom data struct, make sure to override this function.
 *
 **/
void
cpg_operator_free_data (CpgOperator     *op,
                        CpgOperatorData *data)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	if (data)
	{
		CPG_OPERATOR_GET_INTERFACE (op)->free_data (op, data);
	}
}

/**
 * cpg_operator_execute:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 * @stack: A #CpgStack
 *
 * Execute the operator. This function should always be overridden by
 * operator implementations and should always push exactly one number
 * on the stack.
 *
 **/
void
cpg_operator_execute (CpgOperator     *op,
                      CpgOperatorData *data,
                      CpgStack        *stack)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	CPG_OPERATOR_GET_INTERFACE (op)->execute (op, data, stack);
}

/**
 * cpg_operator_get_name:
 * @op: A #CpgOperator
 *
 * Get the operator name. This is the identifier that is used in expressions,
 * and thus can only contain valid identifier characters.
 *
 * Returns: a newly allocated string with the operator name, use #g_free to
 * free the value when it's no longer needed.
 *
 **/
gchar *
cpg_operator_get_name (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_INTERFACE (op)->get_name (op);
}

/**
 * cpg_operator_get_num_arguments:
 * @op: A #CpgOperator
 *
 * Get the number of arguments that the operators expects.
 *
 * Returns: the number of arguments or -1 if the operator accepts a variable
 *          number of arguments.
 *
 **/
gint
cpg_operator_get_num_arguments (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), 0);

	return CPG_OPERATOR_GET_INTERFACE (op)->get_num_arguments (op);
}

/**
 * cpg_operator_reset_cache:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 *
 * Reset the cache of the operator instance.
 *
 **/
void
cpg_operator_reset_cache (CpgOperator     *op,
                          CpgOperatorData *data)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	CPG_OPERATOR_GET_INTERFACE (op)->reset_cache (op, data);
}

/**
 * cpg_operator_reset_variadic:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 *
 * Reset the variadic cache of the operator instance.
 *
 **/
void
cpg_operator_reset_variadic (CpgOperator     *op,
                             CpgOperatorData *data)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	CPG_OPERATOR_GET_INTERFACE (op)->reset_variadic (op, data);
}

/**
 * cpg_operator_get_expressions:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 *
 * Get the expressions that the operator uses.
 *
 * Return value: (element-type CpgExpression) (transfer none): a list of #CpgExpression
 **/
GSList const *
cpg_operator_get_expressions (CpgOperator     *op,
                              CpgOperatorData *data)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_INTERFACE (op)->get_expressions (op, data);
}
