#include "cpg-expression-tree-iter.h"

#include <cpg-network/instructions/cpg-instructions.h>
#include <cpg-network/cpg-math.h>

struct _CpgExpressionTreeIter
{
	CpgInstruction *instruction;
	CpgExpression *expression;

	CpgExpressionTreeIter **children;
	gint num_children;
};

static gchar *
iter_to_string (CpgExpressionTreeIter *iter,
                gint                  *priority,
                gint                  *leftassoc,
                gint                  *comm,
                gboolean               dbg);

static CpgExpressionTreeIter *
iter_new (CpgExpression  *expression,
          CpgInstruction *instruction)
{
	CpgExpressionTreeIter *ret;

	ret = g_slice_new0 (CpgExpressionTreeIter);

	ret->expression = expression;
	ret->instruction = CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (instruction)));

	return ret;
}

void
cpg_expression_tree_iter_free (CpgExpressionTreeIter *self)
{
	if (self->children)
	{
		gint i;

		for (i = 0; i < self->num_children; ++i)
		{
			cpg_expression_tree_iter_free (self->children[i]);
		}

		g_free (self->children);

		self->children = NULL;
		self->num_children = 0;
	}

	cpg_mini_object_free (CPG_MINI_OBJECT (self->instruction));

	g_slice_free (CpgExpressionTreeIter, self);
}

static CpgExpressionTreeIter *
tree_iter_new (CpgExpression *expression,
               GSList const  *instructions)
{
	GQueue stack;

	g_return_val_if_fail (expression == NULL || CPG_IS_EXPRESSION (expression), NULL);

	if (!instructions && expression)
	{
		instructions = cpg_expression_get_instructions (expression);
	}

	g_queue_init (&stack);

	while (instructions)
	{
		CpgExpressionTreeIter *iter;
		CpgInstruction *inst;
		gint cnt;
		gint i;

		inst = instructions->data;
		iter = iter_new (expression, inst);

		cnt = -cpg_instruction_get_stack_count (inst) + 1;

		if (cnt > 0)
		{
			iter->children = g_new (CpgExpressionTreeIter *, cnt);
			iter->num_children = cnt;
		}

		for (i = 0; i < cnt; ++i)
		{
			iter->children[cnt - i - 1] = g_queue_pop_head (&stack);
		}

		g_queue_push_head (&stack, iter);
		instructions = g_slist_next (instructions);
	}

	return g_queue_pop_head (&stack);
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_new (CpgExpression *expression)
{
	return tree_iter_new (expression, NULL);
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_new_from_instructions (GSList const *instructions)
{
	return tree_iter_new (NULL, instructions);
}

CpgExpression *
cpg_expression_tree_iter_get_expression (CpgExpressionTreeIter *iter)
{
	return iter->expression;
}

CpgInstruction *
cpg_expression_tree_iter_get_instruction (CpgExpressionTreeIter *iter)
{
	return iter->instruction;
}

void
cpg_expression_tree_iter_set_instruction (CpgExpressionTreeIter *iter,
                                          CpgInstruction        *instr)
{
	if (iter->instruction)
	{
		cpg_mini_object_free (CPG_MINI_OBJECT (iter->instruction));
		iter->instruction = NULL;
	}

	if (instr)
	{
		iter->instruction = CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (instr)));
	}
}

gint
cpg_expression_tree_iter_num_children (CpgExpressionTreeIter *iter)
{
	return iter->num_children;
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_get_child (CpgExpressionTreeIter *iter,
                                    gint nth)
{
	g_return_val_if_fail (nth >= 0 && nth < iter->num_children, NULL);

	return iter->children[nth];
}

void
cpg_expression_tree_iter_take_child (CpgExpressionTreeIter *iter,
                                     gint                   nth,
                                     CpgExpressionTreeIter *child)
{
	g_return_if_fail (nth >= 0 && nth < iter->num_children);

	cpg_expression_tree_iter_free (iter->children[nth]);
	iter->children[nth] = child;
}

void
cpg_expression_tree_iter_set_child (CpgExpressionTreeIter *iter,
                                    gint                   nth,
                                    CpgExpressionTreeIter *child)
{
	cpg_expression_tree_iter_take_child (iter,
	                                     nth,
	                                     cpg_expression_tree_iter_copy (child));
}

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

	// TODO: resolve property in from/to of link?
	// TODO: resolve property in parent/child?

	prop = cpg_instruction_property_get_property (inst);

	if (dbg)
	{
		g_string_append (ret, cpg_property_get_full_name (prop));
	}
	else
	{
		g_string_append (ret, cpg_property_get_name (prop));
	}

	if (cpg_instruction_property_get_binding (inst) &
	    CPG_INSTRUCTION_PROPERTY_BINDING_DIFF)
	{
		g_string_append_c (ret, '\'');
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
bin_op (GString             *ret,
        gchar const * const *children,
        gchar const         *op)
{
	g_string_append (ret, children[0]);
	g_string_append_c (ret, ' ');
	g_string_append (ret, op);
	g_string_append_c (ret, ' ');
	g_string_append (ret, children[1]);
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
			bin_op (ret, children, "**");
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
append_comma_children (GString             *ret,
                       gchar const * const *children)
{
	gchar const * const *ptr = children;

	while (*ptr)
	{
		if (ptr != children)
		{
			g_string_append (ret, ", ");
		}

		g_string_append (ret, *ptr);
		++ptr;
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

	// TODO: ignore optional arguments on the stack
	append_comma_children (ret, children);

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

	// TODO: ignore implicit and optional arguments on the stack
	append_comma_children (ret, children);

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
custom_operator_to_string_real (CpgOperator         *op,
                                gchar const * const *children,
                                GString             *ret,
                                gboolean             dbg)
{
	GSList const *expr;
	GSList const *expressions;

	g_string_append (ret, cpg_operator_get_name (op));
	g_string_append_c (ret, '[');

	expressions = cpg_operator_get_expressions (op);

	for (expr = expressions; expr; expr = g_slist_next (expr))
	{
		CpgExpressionTreeIter *iter;
		gchar *s;

		if (expr != expressions)
		{
			g_string_append (ret, ", ");
		}

		iter = cpg_expression_tree_iter_new (expr->data);

		s = iter_to_string (iter, NULL, NULL, NULL, dbg);
		g_string_append (ret, s);
		g_free (s);

		cpg_expression_tree_iter_free (iter);
	}

	g_string_append_c (ret, ']');

	if (children)
	{
		g_string_append_c (ret, '(');

		append_comma_children (ret, children);

		g_string_append_c (ret, ')');
	}
}

static void
custom_operator_to_string (CpgInstructionCustomOperator *inst,
                           gchar const * const          *children,
                           GString                      *ret,
                           gboolean                      dbg)
{
	custom_operator_to_string_real (cpg_instruction_custom_operator_get_operator (inst),
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
	custom_operator_to_string_real (cpg_instruction_custom_operator_ref_get_operator (inst),
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

	id = cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (instr));

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
	    cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (parent->instruction)) ==
	    cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (child->instruction)) &&
	    commutative)
	{
		return FALSE;
	}

	return TRUE;
}

static gchar *
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

		gchar *c;

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
			childs[i] = c;
		}
		else
		{
			childs[i] = g_strconcat ("(", c, ")", NULL);
			g_free (c);
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

	return g_string_free (ret, FALSE);
}

gchar *
cpg_expression_tree_iter_to_string (CpgExpressionTreeIter *iter)
{
	return iter_to_string (iter, NULL, NULL, NULL, FALSE);
}

gchar *
cpg_expression_tree_iter_to_string_dbg (CpgExpressionTreeIter *iter)
{
	return iter_to_string (iter, NULL, NULL, NULL, TRUE);
}

static GSList *
iter_to_instructions (CpgExpressionTreeIter *iter,
                      GSList                *ret)
{
	gint i;

	ret = g_slist_prepend (ret, cpg_mini_object_copy (CPG_MINI_OBJECT (iter->instruction)));

	for (i = iter->num_children - 1; i >= 0; --i)
	{
		ret = iter_to_instructions (iter->children[i], ret);
	}

	return ret;
}

GSList *
cpg_expression_tree_iter_to_instructions (CpgExpressionTreeIter *iter)
{
	return iter_to_instructions (iter, NULL);
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_copy (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *cp;
	gint i;

	cp = iter_new (iter->expression, iter->instruction);

	cp->num_children = iter->num_children;
	cp->children = g_new0 (CpgExpressionTreeIter *, cp->num_children + 1);

	for (i = 0; i < cp->num_children; ++i)
	{
		cp->children[i] = cpg_expression_tree_iter_copy (iter->children[i]);
	}

	return cp;
}
