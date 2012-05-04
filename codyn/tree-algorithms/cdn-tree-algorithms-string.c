#include "codyn/cdn-expression-tree-iter.h"
#include "cdn-tree-algorithms-private.h"

static gchar const *
iter_to_string (CdnExpressionTreeIter *iter,
                gint                  *priority,
                gint                  *leftassoc,
                gint                  *comm,
                gboolean               dbg);

typedef void (*InstructionToStringFunc)(CdnInstruction      *instruction,
                                        gchar const * const *children,
                                        GString             *ret,
                                        gboolean             dbg);

static void
property_to_string (CdnInstructionVariable *inst,
                    gchar const * const    *children,
                    GString                *ret,
                    gboolean                dbg)
{
	CdnVariable *prop;
	CdnInstructionVariableBinding binding;

	binding = cdn_instruction_variable_get_binding (inst);

	// TODO: resolve property in parent/child?

	if (binding & CDN_INSTRUCTION_VARIABLE_BINDING_INPUT)
	{
		g_string_append (ret, "input.");
	}
	else if (binding & CDN_INSTRUCTION_VARIABLE_BINDING_OUTPUT)
	{
		g_string_append (ret, "output.");
	}

	prop = cdn_instruction_variable_get_variable (inst);

	if (dbg)
	{
		g_string_append (ret, cdn_variable_get_full_name (prop));
	}
	else
	{
		g_string_append (ret, cdn_variable_get_name (prop));
	}
}

static void
number_to_string (CdnInstructionNumber *inst,
                  gchar const * const  *children,
                  GString              *ret,
                  gboolean                         dbg)
{
	gchar *s;

	s = cdn_instruction_number_get_representation (inst);
	g_string_append (ret, s);

	g_free (s);
}

static void
bin_op_sp (GString             *ret,
           gchar const * const *children,
           gchar const         *op,
           gboolean             sp)
{
	g_string_append (ret, children[0]);

	if (sp)
	{
		g_string_append_c (ret, ' ');
	}

	g_string_append (ret, op);

	if (sp)
	{
		g_string_append_c (ret, ' ');
	}

	g_string_append (ret, children[1]);
}

static void
bin_op (GString             *ret,
        gchar const * const *children,
        gchar const         *op)
{
	return bin_op_sp (ret, children, op, TRUE);
}

static void
operator_to_string (CdnInstructionFunction  *inst,
                    gchar const * const     *children,
                    GString                 *ret,
                    gboolean                 dbg)
{
	switch (cdn_instruction_function_get_id (inst))
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
			g_string_append_c (ret, '-');
			g_string_append (ret, children[0]);
		break;
		case CDN_MATH_FUNCTION_TYPE_MINUS:
			bin_op (ret, children, "-");
		break;
		case CDN_MATH_FUNCTION_TYPE_PLUS:
			bin_op (ret, children, "+");
		break;
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			bin_op (ret, children, "*");
		break;
		case CDN_MATH_FUNCTION_TYPE_DIVIDE:
			bin_op (ret, children, "/");
		break;
		case CDN_MATH_FUNCTION_TYPE_MODULO:
			bin_op (ret, children, "%");
		break;
		case CDN_MATH_FUNCTION_TYPE_POWER:
			bin_op_sp (ret, children, "^", FALSE);
		break;
		case CDN_MATH_FUNCTION_TYPE_GREATER:
			bin_op (ret, children, ">");
		break;
		case CDN_MATH_FUNCTION_TYPE_LESS:
			bin_op (ret, children, "<");
		break;
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
			bin_op (ret, children, ">=");
		break;
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
			bin_op (ret, children, "<=");
		break;
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
			bin_op (ret, children, "==");
		break;
		case CDN_MATH_FUNCTION_TYPE_OR:
			bin_op (ret, children, "||");
		break;
		case CDN_MATH_FUNCTION_TYPE_AND:
			bin_op (ret, children, "&&");
		break;
		case CDN_MATH_FUNCTION_TYPE_NEGATE:
			g_string_append_c (ret, '!');
			g_string_append (ret, children[0]);
		break;
		case CDN_MATH_FUNCTION_TYPE_TERNARY:
			g_string_append_printf (ret,
			                        "%s ? %s : %s",
			                        children[0],
			                        children[1],
			                        children[2]);
		break;
	}
}

static void
append_comma_children (CdnInstruction      *instr,
                       GString             *ret,
                       gchar const * const *children)
{
	gchar const * const *ptr = children;
	GList const *args = NULL;

	if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (instr))
	{
		CdnFunction *f;

		f = cdn_instruction_custom_function_get_function ((CdnInstructionCustomFunction *)(instr));
		args = cdn_function_get_arguments (f);
	}

	while (*ptr)
	{
		if (!args || cdn_function_argument_get_explicit (args->data))
		{
			if (ptr != children)
			{
				g_string_append (ret, ", ");
			}

			g_string_append (ret, *ptr);
		}

		++ptr;

		args = g_list_next (args);
	}
}

static void
index_to_string (CdnInstructionFunction *inst,
                 gchar const * const    *children,
                 GString                *ret,
                 gboolean                dbg)
{
	if (!children[2])
	{
		g_string_append_printf (ret, "(%s)[%s]", children[1], children[0]);
	}
	else
	{
		g_string_append_printf (ret,
		                        "(%s)[%s, %s]",
		                        children[2],
		                        children[0],
		                        children[1]);
	}
}

static void
function_to_string (CdnInstructionFunction *inst,
                    gchar const * const    *children,
                    GString                *ret,
                    gboolean                dbg)
{
	gchar const *name;
	gint id;

	id = cdn_instruction_function_get_id (inst);

	if (id < CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS)
	{
		operator_to_string (inst, children, ret, dbg);
		return;
	}

	if (id == CDN_MATH_FUNCTION_TYPE_INDEX)
	{
		index_to_string (inst, children, ret, dbg);
		return;
	}
	else if (id == CDN_MATH_FUNCTION_TYPE_TRANSPOSE)
	{
		g_string_append (ret, *children);
		g_string_append (ret, "\xe1\xb5\x80");
		return;
	}

	name = cdn_instruction_function_get_name (inst);

	g_string_append (ret, name);
	g_string_append_c (ret, '(');

	append_comma_children (CDN_INSTRUCTION (inst), ret, children);

	g_string_append_c (ret, ')');
}

static void
custom_function_to_string (CdnInstructionCustomFunction *inst,
                           gchar const * const          *children,
                           GString                      *ret,
                           gboolean                      dbg)
{
	CdnFunction *func;

	func = cdn_instruction_custom_function_get_function (inst);

	g_string_append (ret, cdn_object_get_id (CDN_OBJECT (func)));
	g_string_append_c (ret, '(');

	append_comma_children (CDN_INSTRUCTION (inst), ret, children);

	g_string_append_c (ret, ')');
}

static void
custom_function_ref_to_string (CdnInstructionCustomFunctionRef *inst,
                               gchar const * const             *children,
                               GString                         *ret,
                               gboolean                         dbg)
{
	CdnFunction *func;

	func = cdn_instruction_custom_function_ref_get_function (inst);
	g_string_append (ret, cdn_object_get_id (CDN_OBJECT (func)));
}

static void
custom_operator_expressions_to_string (GSList const *expressions,
                                       gboolean      dbg,
                                       GString      *ret)
{
	GSList const *expr;

	for (expr = expressions; expr; expr = g_slist_next (expr))
	{
		CdnExpressionTreeIter *iter;
		gchar const *s;

		if (expr != expressions)
		{
			g_string_append (ret, ", ");
		}

		iter = cdn_expression_tree_iter_new (expr->data);

		s = iter_to_string (iter, NULL, NULL, NULL, dbg);
		g_string_append (ret, s);

		cdn_expression_tree_iter_free (iter);
	}
}

static void
custom_operator_to_string_real (CdnInstruction      *inst,
                                CdnOperator         *op,
                                gchar const * const *children,
                                GString             *ret,
                                gboolean             dbg)
{
	gint i;
	gint num;

	g_string_append (ret, cdn_operator_get_name (op));
	g_string_append_c (ret, '[');

	num = cdn_operator_num_expressions (op);

	for (i = 0; i < num; ++i)
	{
		if (i != 0)
		{
			g_string_append (ret, "; ");
		}

		custom_operator_expressions_to_string (cdn_operator_get_expressions (op, i),
		                                       dbg,
		                                       ret);
	}

	g_string_append_c (ret, ']');

	num = cdn_operator_num_indices (op);

	if (num > 0)
	{
		g_string_append_c (ret, '[');

		for (i = 0; i < num; ++i)
		{
			if (i != 0)
			{
				g_string_append (ret, "; ");
			}

			custom_operator_expressions_to_string (cdn_operator_get_indices (op, i),
			                                       dbg,
			                                       ret);
		}

		g_string_append_c (ret, ']');
	}

	if (children && *children)
	{
		g_string_append_c (ret, '(');

		append_comma_children (inst, ret, children);

		g_string_append_c (ret, ')');
	}
}

static void
custom_operator_to_string (CdnInstructionCustomOperator *inst,
                           gchar const * const          *children,
                           GString                      *ret,
                           gboolean                      dbg)
{
	custom_operator_to_string_real (CDN_INSTRUCTION (inst),
	                                cdn_instruction_custom_operator_get_operator (inst),
	                                children,
	                                ret,
	                                dbg);
}

static void
custom_operator_ref_to_string (CdnInstructionCustomOperatorRef *inst,
                               gchar const * const             *children,
                               GString                         *ret,
                               gboolean                         dbg)
{
	custom_operator_to_string_real (CDN_INSTRUCTION (inst),
	                                cdn_instruction_custom_operator_ref_get_operator (inst),
	                                children,
	                                ret,
	                                dbg);
}

static void
matrix_to_string (CdnInstructionMatrix *inst,
                  gchar const * const  *children,
                  GString              *ret,
                  gboolean              dbg)
{
	CdnStackManipulation const *smanip;
	CdnDimension dim;
	gint i = 0;
	gint accumnumc = 0;

	g_string_append_c (ret, '[');

	smanip = cdn_instruction_get_stack_manipulation (CDN_INSTRUCTION (inst), NULL);
	dim = smanip->push.dimension;

	while (*children)
	{
		CdnDimension cdim;

		cdim = smanip->pop.args[i].dimension;

		g_string_append (ret, *children);
		accumnumc += cdim.columns;

		++i;
		++children;

		if (accumnumc == dim.columns)
		{
			if (*children)
			{
				g_string_append (ret, "; ");
			}

			accumnumc = 0;
		}
		else if (*children)
		{
			g_string_append (ret, ", ");
		}

	}

	g_string_append_c (ret, ']');
}

static InstructionToStringFunc
to_string_func (CdnInstruction *instruction)
{
	if (CDN_IS_INSTRUCTION_VARIABLE (instruction))
	{
		return (InstructionToStringFunc)property_to_string;
	}
	else if (CDN_IS_INSTRUCTION_NUMBER (instruction))
	{
		return (InstructionToStringFunc)number_to_string;
	}
	else if (CDN_IS_INSTRUCTION_FUNCTION (instruction))
	{
		return (InstructionToStringFunc)function_to_string;
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (instruction))
	{
		return (InstructionToStringFunc)custom_function_to_string;
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instruction))
	{
		return (InstructionToStringFunc)custom_function_ref_to_string;
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR (instruction))
	{
		return (InstructionToStringFunc)custom_operator_to_string;
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instruction))
	{
		return (InstructionToStringFunc)custom_operator_ref_to_string;
	}
	else if (CDN_IS_INSTRUCTION_MATRIX (instruction))
	{
		return (InstructionToStringFunc)matrix_to_string;
	}

	return NULL;
}

static gboolean
instruction_is_operator (CdnInstruction *instruction)
{
	if (!CDN_IS_INSTRUCTION_FUNCTION (instruction))
	{
		return FALSE;
	}

	return cdn_instruction_function_get_id (CDN_INSTRUCTION_FUNCTION (instruction)) <
	       CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS;
}

static void
instruction_priority (CdnInstruction *instr,
                      gint           *priority,
                      gint           *leftassoc,
                      gint           *commutative)
{
	gint id;

	*leftassoc = 1;
	*priority = 1000;
	*commutative = 0;

	if (!instruction_is_operator (instr))
	{
		return;
	}

	id = cdn_instruction_function_get_id ((CdnInstructionFunction *)(instr));

	switch (id)
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		case CDN_MATH_FUNCTION_TYPE_NEGATE:
		case CDN_MATH_FUNCTION_TYPE_INDEX:
			*priority = 8;
			*leftassoc = 0;
		break;
		case CDN_MATH_FUNCTION_TYPE_PLUS: // Smart fallthrough
			*commutative = 1;
		case CDN_MATH_FUNCTION_TYPE_MINUS:
			*priority = 6;
		break;
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			*commutative = 1;
		case CDN_MATH_FUNCTION_TYPE_DIVIDE:
		case CDN_MATH_FUNCTION_TYPE_MODULO:
			*priority = 7;
		break;
		case CDN_MATH_FUNCTION_TYPE_POWER:
			*priority = 9;
			*leftassoc = 0;
		break;
		case CDN_MATH_FUNCTION_TYPE_GREATER:
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
			*priority = 5;
		break;
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
			*commutative = 1;
			*priority = 4;
		break;
		case CDN_MATH_FUNCTION_TYPE_OR:
			*commutative = 1;
			*priority = 2;
		break;
		case CDN_MATH_FUNCTION_TYPE_AND:
			*commutative = 1;
			*priority = 3;
		break;
		case CDN_MATH_FUNCTION_TYPE_TERNARY:
			*priority = 1;
		break;
	}
}

static gboolean
needs_paren (CdnExpressionTreeIter *parent,
             CdnExpressionTreeIter *child,
             gint                   idx,
             gint                   pprio,
             gint                   cprio,
             gint                   lassoc,
             gint                   commutative)
{
	CdnInstruction *instr;

	instr = cdn_expression_tree_iter_get_instruction (parent);

	if (!instruction_is_operator (instr))
	{
		return FALSE;
	}

	if (cprio > pprio)
	{
		return FALSE;
	}

	if (cprio == pprio && ((lassoc && idx == 0) || (!lassoc && idx != 0)))
	{
		return FALSE;
	}

	if (cprio == pprio &&
	    instruction_is_operator (parent->instruction) &&
	    instruction_is_operator (child->instruction) &&
	    cdn_instruction_function_get_id ((CdnInstructionFunction *)(parent->instruction)) ==
	    cdn_instruction_function_get_id ((CdnInstructionFunction *)(child->instruction)) &&
	    commutative)
	{
		return FALSE;
	}

	return TRUE;
}

static gchar const *
iter_to_string (CdnExpressionTreeIter *iter,
                gint                  *priority,
                gint                  *leftassoc,
                gint                  *comm,
                gboolean               dbg)
{
	GString *ret;
	gchar **childs = NULL;
	InstructionToStringFunc func;
	gint i;
	gint iprio;
	gint ilassoc;
	gint icomm;

	if (priority)
	{
		*priority = 0;
	}

	if (leftassoc)
	{
		*leftassoc = 1;
	}

	if (comm)
	{
		*comm = 0;
	}

	if (!iter)
	{
		return g_strdup ("(null)");
	}

	if (iter->cached_to_string && iter->cache_is_dbg == dbg)
	{
		return iter->cached_to_string;
	}

	g_free (iter->cached_to_string);
	iter->cached_to_string = NULL;

	ret = g_string_new ("");

	childs = g_new0 (gchar *, iter->num_children + 1);

	instruction_priority (iter->instruction, &iprio, &ilassoc, &icomm);

	if (priority)
	{
		*priority = iprio;
	}

	if (leftassoc)
	{
		*leftassoc = ilassoc;
	}

	if (comm)
	{
		*comm = icomm;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		gint cprio;
		gint classoc;
		gint ccomm;
		gchar const *c;

		c = iter_to_string (iter->children[i],
		                    &cprio,
		                    &classoc,
		                    &ccomm,
		                    dbg);

		if (!needs_paren (iter,
		                  iter->children[i],
		                  i,
		                  iprio,
		                  cprio,
		                  classoc,
		                  ccomm))
		{
			childs[i] = g_strdup (c);
		}
		else
		{
			childs[i] = g_strconcat ("(", c, ")", NULL);
		}
	}

	func = to_string_func (iter->instruction);

	if (func)
	{
		func (iter->instruction,
		      (gchar const * const *)childs,
		      ret,
		      dbg);
	}

	g_strfreev (childs);

	iter->cached_to_string = g_string_free (ret, FALSE);
	iter->cache_is_dbg = dbg;

	return iter->cached_to_string;
}

gchar const *
cdn_expression_tree_iter_to_string (CdnExpressionTreeIter *iter)
{
	return iter_to_string (iter, NULL, NULL, NULL, FALSE);
}

gchar const *
cdn_expression_tree_iter_to_string_dbg (CdnExpressionTreeIter *iter)
{
	return iter_to_string (iter, NULL, NULL, NULL, TRUE);
}
