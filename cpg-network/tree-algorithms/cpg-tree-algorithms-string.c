#include "cpg-network/cpg-expression-tree-iter.h"
#include "cpg-tree-algorithms-private.h"

static gchar const *
iter_to_string (CpgExpressionTreeIter *iter,
                gint                  *priority,
                gint                  *leftassoc,
                gint                  *comm,
                gboolean               dbg);

typedef void (*InstructionToStringFunc)(CpgInstruction      *instruction,
                                        gchar const * const *children,
                                        GString             *ret,
                                        gboolean             dbg);

static void
property_to_string (CpgInstructionProperty *inst,
                    gchar const * const    *children,
                    GString                *ret,
                    gboolean                dbg)
{
	CpgProperty *prop;
	CpgInstructionPropertyBinding binding;

	binding = cpg_instruction_property_get_binding (inst);

	// TODO: resolve property in parent/child?

	if (binding & CPG_INSTRUCTION_PROPERTY_BINDING_FROM)
	{
		g_string_append (ret, "from.");
	}
	else if (binding & CPG_INSTRUCTION_PROPERTY_BINDING_TO)
	{
		g_string_append (ret, "to.");
	}

	prop = cpg_instruction_property_get_property (inst);

	if (dbg)
	{
		g_string_append (ret, cpg_property_get_full_name (prop));
	}
	else
	{
		g_string_append (ret, cpg_property_get_name (prop));
	}
}

static void
constant_to_string (CpgInstructionConstant *inst,
                    gchar const * const    *children,
                    GString                *ret,
                    gboolean                dbg)
{
	g_string_append (ret, cpg_instruction_constant_get_symbol (inst));
}

static void
number_to_string (CpgInstructionNumber *inst,
                  gchar const * const  *children,
                  GString              *ret,
                  gboolean                         dbg)
{
	gchar *s;

	s = cpg_instruction_number_get_representation (inst);
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
operator_to_string (CpgInstructionFunction  *inst,
                    gchar const * const     *children,
                    GString                 *ret,
                    gboolean                         dbg)
{
	switch (cpg_instruction_function_get_id (inst))
	{
		case CPG_MATH_OPERATOR_TYPE_UNARY_MINUS:
			g_string_append_c (ret, '-');
			g_string_append (ret, children[0]);
		break;
		case CPG_MATH_OPERATOR_TYPE_MINUS:
			bin_op (ret, children, "-");
		break;
		case CPG_MATH_OPERATOR_TYPE_PLUS:
			bin_op (ret, children, "+");
		break;
		case CPG_MATH_OPERATOR_TYPE_MULTIPLY:
			bin_op (ret, children, "*");
		break;
		case CPG_MATH_OPERATOR_TYPE_DIVIDE:
			bin_op (ret, children, "/");
		break;
		case CPG_MATH_OPERATOR_TYPE_MODULO:
			bin_op (ret, children, "%");
		break;
		case CPG_MATH_OPERATOR_TYPE_POWER:
			bin_op_sp (ret, children, "^", FALSE);
		break;
		case CPG_MATH_OPERATOR_TYPE_GREATER:
			bin_op (ret, children, ">");
		break;
		case CPG_MATH_OPERATOR_TYPE_LESS:
			bin_op (ret, children, "<");
		break;
		case CPG_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL:
			bin_op (ret, children, ">=");
		break;
		case CPG_MATH_OPERATOR_TYPE_LESS_OR_EQUAL:
			bin_op (ret, children, "<=");
		break;
		case CPG_MATH_OPERATOR_TYPE_EQUAL:
			bin_op (ret, children, "==");
		break;
		case CPG_MATH_OPERATOR_TYPE_OR:
			bin_op (ret, children, "||");
		break;
		case CPG_MATH_OPERATOR_TYPE_AND:
			bin_op (ret, children, "&&");
		break;
		case CPG_MATH_OPERATOR_TYPE_NEGATE:
			g_string_append_c (ret, '!');
			g_string_append (ret, children[0]);
		break;
		case CPG_MATH_OPERATOR_TYPE_TERNARY:
			g_string_append_printf (ret,
			                        "%s ? %s : %s",
			                        children[0],
			                        children[1],
			                        children[2]);
		break;
	}
}

static void
append_comma_children (CpgInstruction      *instr,
                       GString             *ret,
                       gchar const * const *children)
{
	gchar const * const *ptr = children;
	GList const *args = NULL;

	if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (instr))
	{
		CpgFunction *f;

		f = cpg_instruction_custom_function_get_function ((CpgInstructionCustomFunction *)(instr));
		args = cpg_function_get_arguments (f);
	}

	while (*ptr)
	{
		if (!args || cpg_function_argument_get_explicit (args->data))
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
function_to_string (CpgInstructionFunction *inst,
                    gchar const * const    *children,
                    GString                *ret,
                    gboolean                dbg)
{
	gchar const *name;

	name = cpg_instruction_function_get_name (inst);

	g_string_append (ret, name);
	g_string_append_c (ret, '(');

	append_comma_children (CPG_INSTRUCTION (inst), ret, children);

	g_string_append_c (ret, ')');
}

static void
custom_function_to_string (CpgInstructionCustomFunction *inst,
                           gchar const * const          *children,
                           GString                      *ret,
                           gboolean                      dbg)
{
	CpgFunction *func;

	func = cpg_instruction_custom_function_get_function (inst);

	g_string_append (ret, cpg_object_get_id (CPG_OBJECT (func)));
	g_string_append_c (ret, '(');

	append_comma_children (CPG_INSTRUCTION (inst), ret, children);

	g_string_append_c (ret, ')');
}

static void
custom_function_ref_to_string (CpgInstructionCustomFunctionRef *inst,
                               gchar const * const             *children,
                               GString                         *ret,
                               gboolean                         dbg)
{
	CpgFunction *func;

	func = cpg_instruction_custom_function_ref_get_function (inst);
	g_string_append (ret, cpg_object_get_id (CPG_OBJECT (func)));
}

static void
custom_operator_expressions_to_string (GSList const *expressions,
                                       gboolean      dbg,
                                       GString      *ret)
{
	GSList const *expr;

	for (expr = expressions; expr; expr = g_slist_next (expr))
	{
		CpgExpressionTreeIter *iter;
		gchar const *s;

		if (expr != expressions)
		{
			g_string_append (ret, ", ");
		}

		iter = cpg_expression_tree_iter_new (expr->data);

		s = iter_to_string (iter, NULL, NULL, NULL, dbg);
		g_string_append (ret, s);

		cpg_expression_tree_iter_free (iter);
	}
}

static void
custom_operator_to_string_real (CpgInstruction      *inst,
                                CpgOperator         *op,
                                gchar const * const *children,
                                GString             *ret,
                                gboolean             dbg)
{
	gint i;
	gint num;

	g_string_append (ret, cpg_operator_get_name (op));
	g_string_append_c (ret, '[');

	num = cpg_operator_num_expressions (op);

	for (i = 0; i < num; ++i)
	{
		if (i != 0)
		{
			g_string_append (ret, "; ");
		}

		custom_operator_expressions_to_string (cpg_operator_get_expressions (op, i),
		                                       dbg,
		                                       ret);
	}

	g_string_append_c (ret, ']');

	num = cpg_operator_num_indices (op);

	if (num > 0)
	{
		g_string_append_c (ret, '[');

		for (i = 0; i < num; ++i)
		{
			if (i != 0)
			{
				g_string_append (ret, "; ");
			}

			custom_operator_expressions_to_string (cpg_operator_get_indices (op, i),
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
custom_operator_to_string (CpgInstructionCustomOperator *inst,
                           gchar const * const          *children,
                           GString                      *ret,
                           gboolean                      dbg)
{
	custom_operator_to_string_real (CPG_INSTRUCTION (inst),
	                                cpg_instruction_custom_operator_get_operator (inst),
	                                children,
	                                ret,
	                                dbg);
}

static void
custom_operator_ref_to_string (CpgInstructionCustomOperatorRef *inst,
                               gchar const * const             *children,
                               GString                         *ret,
                               gboolean                         dbg)
{
	custom_operator_to_string_real (CPG_INSTRUCTION (inst),
	                                cpg_instruction_custom_operator_ref_get_operator (inst),
	                                children,
	                                ret,
	                                dbg);
}

static InstructionToStringFunc
to_string_func (CpgInstruction *instruction)
{
	if (CPG_IS_INSTRUCTION_PROPERTY (instruction))
	{
		return (InstructionToStringFunc)property_to_string;
	}
	else if (CPG_IS_INSTRUCTION_CONSTANT (instruction))
	{
		return (InstructionToStringFunc)constant_to_string;
	}
	else if (CPG_IS_INSTRUCTION_NUMBER (instruction))
	{
		return (InstructionToStringFunc)number_to_string;
	}
	else if (CPG_IS_INSTRUCTION_OPERATOR (instruction))
	{
		return (InstructionToStringFunc)operator_to_string;
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (instruction))
	{
		return (InstructionToStringFunc)custom_function_to_string;
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instruction))
	{
		return (InstructionToStringFunc)custom_function_ref_to_string;
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (instruction))
	{
		return (InstructionToStringFunc)custom_operator_to_string;
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instruction))
	{
		return (InstructionToStringFunc)custom_operator_ref_to_string;
	}
	else if (CPG_IS_INSTRUCTION_FUNCTION (instruction))
	{
		return (InstructionToStringFunc)function_to_string;
	}

	return NULL;
}

static void
instruction_priority (CpgInstruction *instr,
                      gint           *priority,
                      gint           *leftassoc,
                      gint           *commutative)
{
	gint id;

	*leftassoc = 1;
	*priority = 1000;
	*commutative = 0;

	if (!CPG_IS_INSTRUCTION_OPERATOR (instr))
	{
		return;
	}

	id = cpg_instruction_function_get_id ((CpgInstructionFunction *)(instr));

	switch (id)
	{
		case CPG_MATH_OPERATOR_TYPE_UNARY_MINUS:
		case CPG_MATH_OPERATOR_TYPE_NEGATE:
			*priority = 8;
			*leftassoc = 0;
		break;
		case CPG_MATH_OPERATOR_TYPE_PLUS: // Smart fallthrough
			*commutative = 1;
		case CPG_MATH_OPERATOR_TYPE_MINUS:
			*priority = 6;
		break;
		case CPG_MATH_OPERATOR_TYPE_MULTIPLY:
			*commutative = 1;
		case CPG_MATH_OPERATOR_TYPE_DIVIDE:
		case CPG_MATH_OPERATOR_TYPE_MODULO:
			*priority = 7;
		break;
		case CPG_MATH_OPERATOR_TYPE_POWER:
			*priority = 9;
			*leftassoc = 0;
		break;
		case CPG_MATH_OPERATOR_TYPE_GREATER:
		case CPG_MATH_OPERATOR_TYPE_LESS:
		case CPG_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL:
		case CPG_MATH_OPERATOR_TYPE_LESS_OR_EQUAL:
			*priority = 5;
		break;
		case CPG_MATH_OPERATOR_TYPE_EQUAL:
			*commutative = 1;
			*priority = 4;
		break;
		case CPG_MATH_OPERATOR_TYPE_OR:
			*commutative = 1;
			*priority = 2;
		break;
		case CPG_MATH_OPERATOR_TYPE_AND:
			*commutative = 1;
			*priority = 3;
		break;
		case CPG_MATH_OPERATOR_TYPE_TERNARY:
			*priority = 1;
		break;
	}
}

static gboolean
needs_paren (CpgExpressionTreeIter *parent,
             CpgExpressionTreeIter *child,
             gint                   idx,
             gint                   pprio,
             gint                   cprio,
             gint                   lassoc,
             gint                   commutative)
{
	CpgInstruction *instr;

	instr = cpg_expression_tree_iter_get_instruction (parent);

	if (!CPG_IS_INSTRUCTION_OPERATOR (instr))
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
	    CPG_IS_INSTRUCTION_OPERATOR (parent->instruction) &&
	    CPG_IS_INSTRUCTION_OPERATOR (child->instruction) &&
	    cpg_instruction_function_get_id ((CpgInstructionFunction *)(parent->instruction)) ==
	    cpg_instruction_function_get_id ((CpgInstructionFunction *)(child->instruction)) &&
	    commutative)
	{
		return FALSE;
	}

	return TRUE;
}

static gchar const *
iter_to_string (CpgExpressionTreeIter *iter,
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
cpg_expression_tree_iter_to_string (CpgExpressionTreeIter *iter)
{
	return iter_to_string (iter, NULL, NULL, NULL, FALSE);
}

gchar const *
cpg_expression_tree_iter_to_string_dbg (CpgExpressionTreeIter *iter)
{
	return iter_to_string (iter, NULL, NULL, NULL, TRUE);
}
