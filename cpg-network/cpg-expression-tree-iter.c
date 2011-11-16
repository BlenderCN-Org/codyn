#include "cpg-expression-tree-iter.h"

#include <cpg-network/instructions/cpg-instructions.h>
#include <cpg-network/cpg-math.h>
#include <math.h>
#include "cpg-stack-private.h"
#include "cpg-operators.h"
#include "cpg-debug.h"
#include "cpg-symbolic.h"

struct _CpgExpressionTreeIter
{
	CpgExpressionTreeIter *parent;
	CpgInstruction *instruction;

	CpgExpressionTreeIter **children;
	gint num_children;
	gint depth;
};

static CpgExpressionTreeIter *iter_simplify (CpgExpressionTreeIter *iter,
                                             gboolean               simplify_children);

static gchar *
iter_to_string (CpgExpressionTreeIter *iter,
                gint                  *priority,
                gint                  *leftassoc,
                gint                  *comm,
                gboolean               dbg);

static CpgExpressionTreeIter *
iter_new (CpgInstruction *instruction)
{
	CpgExpressionTreeIter *ret;

	ret = g_slice_new0 (CpgExpressionTreeIter);

	ret->instruction = CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (instruction)));

	return ret;
}

static CpgExpressionTreeIter *
iter_new_sized (CpgInstruction *instruction, gint num)
{
	CpgExpressionTreeIter *ret;

	ret = iter_new (instruction);
	ret->num_children = num;

	ret->children = g_new0 (CpgExpressionTreeIter *, num);
	return ret;
}

void
cpg_expression_tree_iter_free (CpgExpressionTreeIter *self)
{
	if (!self)
	{
		return;
	}

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

	if (self->instruction)
	{
		cpg_mini_object_free (CPG_MINI_OBJECT (self->instruction));
	}

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
		iter = iter_new (inst);

		cnt = -cpg_instruction_get_stack_count (inst) + 1;

		if (cnt > 0)
		{
			iter->children = g_new (CpgExpressionTreeIter *, cnt);
			iter->num_children = cnt;
		}

		for (i = 0; i < cnt; ++i)
		{
			CpgExpressionTreeIter *child;

			child = g_queue_pop_head (&stack);

			child->parent = iter;
			iter->children[cnt - i - 1] = child;

			if (child->depth + 1 > iter->depth)
			{
				iter->depth = child->depth + 1;
			}
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

static void
update_depth (CpgExpressionTreeIter *iter,
              CpgExpressionTreeIter *child)
{
	if (child->depth + 1 > iter->depth)
	{
		iter->depth = child->depth + 1;

		update_depth (iter->parent, iter);
	}
}

void
cpg_expression_tree_iter_take_child (CpgExpressionTreeIter *iter,
                                     gint                   nth,
                                     CpgExpressionTreeIter *child)
{
	g_return_if_fail (nth >= 0 && nth < iter->num_children);

	cpg_expression_tree_iter_free (iter->children[nth]);
	iter->children[nth] = child;

	update_depth (iter, child);
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
append_comma_children (CpgInstruction      *instr,
                       GString             *ret,
                       gchar const * const *children)
{
	gchar const * const *ptr = children;
	GList const *args = NULL;

	if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (instr))
	{
		CpgFunction *f;

		f = cpg_instruction_custom_function_get_function (CPG_INSTRUCTION_CUSTOM_FUNCTION (instr));
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

	if (children)
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

	if (!iter)
	{
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

		return g_strdup ("(null)");
	}

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

	if (!iter)
	{
		return NULL;
	}

	cp = iter_new_sized (iter->instruction, iter->num_children);

	for (i = 0; i < cp->num_children; ++i)
	{
		cp->children[i] = cpg_expression_tree_iter_copy (iter->children[i]);

		if (cp->children[i])
		{
			cp->children[i]->parent = cp;
		}
	}

	cp->depth = iter->depth;
	return cp;

}

static gint
compare_custom_function_real (CpgFunction *f1,
                              CpgFunction *f2)
{
	CpgObject *p1;
	CpgObject *p2;

	p1 = cpg_object_get_parent (CPG_OBJECT (f1));
	p2 = cpg_object_get_parent (CPG_OBJECT (f2));

	if (p1 != p2)
	{
		return p1 > p2 ? -1 : 1;
	}

	return g_strcmp0 (cpg_object_get_id (CPG_OBJECT (f1)),
	                  cpg_object_get_id (CPG_OBJECT (f2)));
}

static gint
compare_custom_function (CpgExpressionTreeIter const *iter1,
                         CpgExpressionTreeIter const *iter2)
{
	CpgFunction *f1;
	CpgFunction *f2;

	f1 = cpg_instruction_custom_function_get_function (CPG_INSTRUCTION_CUSTOM_FUNCTION (iter1->instruction));

	f2 = cpg_instruction_custom_function_get_function (CPG_INSTRUCTION_CUSTOM_FUNCTION (iter2->instruction));

	return compare_custom_function_real (f1, f2);

}

static gint
compare_custom_function_ref (CpgExpressionTreeIter const *iter1,
                             CpgExpressionTreeIter const *iter2)
{
	CpgFunction *f1;
	CpgFunction *f2;

	f1 = cpg_instruction_custom_function_ref_get_function (CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (iter1->instruction));

	f2 = cpg_instruction_custom_function_ref_get_function (CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (iter2->instruction));

	return compare_custom_function_real (f1, f2);
}

static gint
compare_custom_operator_real (CpgOperator *o1,
                              CpgOperator *o2)
{
	GType g1;
	GType g2;

	g1 = G_OBJECT_TYPE (o1);
	g2 = G_OBJECT_TYPE (o2);

	return g1 > g2 ? -1 : (g1 < g2 ? 1 : 0);
}

static gint
compare_custom_operator (CpgExpressionTreeIter const *iter1,
                         CpgExpressionTreeIter const *iter2)
{
	CpgOperator *o1;
	CpgOperator *o2;

	o1 = cpg_instruction_custom_operator_get_operator (CPG_INSTRUCTION_CUSTOM_OPERATOR (iter1->instruction));
	o2 = cpg_instruction_custom_operator_get_operator (CPG_INSTRUCTION_CUSTOM_OPERATOR (iter2->instruction));

	return compare_custom_operator_real (o1, o2);
}

static gint
compare_custom_operator_ref (CpgExpressionTreeIter const *iter1,
                             CpgExpressionTreeIter const *iter2)
{
	CpgOperator *o1;
	CpgOperator *o2;

	o1 = cpg_instruction_custom_operator_ref_get_operator (CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (iter1->instruction));
	o2 = cpg_instruction_custom_operator_ref_get_operator (CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (iter2->instruction));

	return compare_custom_operator_real (o1, o2);
}

static gint
compare_function (CpgExpressionTreeIter const *iter1,
                  CpgExpressionTreeIter const *iter2)
{
	gint i1;
	gint i2;

	i1 = cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (iter1->instruction));
	i2 = cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (iter2->instruction));

	return i1 > i2 ? -1 : (i1 < i2 ? 1 : 0);
}

static gint
compare_number (CpgExpressionTreeIter const *iter1,
                CpgExpressionTreeIter const *iter2)
{
	gdouble n1;
	gdouble n2;

	n1 = cpg_instruction_number_get_value (CPG_INSTRUCTION_NUMBER (iter1->instruction));
	n2 = cpg_instruction_number_get_value (CPG_INSTRUCTION_NUMBER (iter2->instruction));

	return n1 > n2 ? 1 : -1;
}

static gint
compare_property (CpgExpressionTreeIter const *iter1,
                  CpgExpressionTreeIter const *iter2)
{
	CpgProperty *p1;
	CpgProperty *p2;

	CpgObject *o1;
	CpgObject *o2;

	p1 = cpg_instruction_property_get_property (CPG_INSTRUCTION_PROPERTY (iter1->instruction));
	p2 = cpg_instruction_property_get_property (CPG_INSTRUCTION_PROPERTY (iter2->instruction));

	o1 = cpg_property_get_object (p1);
	o2 = cpg_property_get_object (p2);

	if (o1 != o2)
	{
		return o1 > o2 ? -1 : 1;
	}

	return g_strcmp0 (cpg_property_get_name (p1),
	                  cpg_property_get_name (p2));
}

static gint
type_id (CpgInstruction *instr)
{
	GType order[] = {
		CPG_TYPE_INSTRUCTION_NUMBER,
		CPG_TYPE_INSTRUCTION_CONSTANT,
		CPG_TYPE_INSTRUCTION_PROPERTY,
		CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION,
		CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF,
		CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR,
		CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF,
		CPG_TYPE_INSTRUCTION_FUNCTION,
		CPG_TYPE_INSTRUCTION_OPERATOR,
		G_TYPE_INVALID
	};

	gint i = 0;
	GType t = G_OBJECT_TYPE (instr);

	while (order[i] != G_TYPE_INVALID)
	{
		if (t == order[i])
		{
			return i;
		}

		++i;
	}

	return -1;
}

static gint
compare_iters (CpgExpressionTreeIter **i1,
               CpgExpressionTreeIter **i2)
{
	CpgExpressionTreeIter const *iter1;
	CpgExpressionTreeIter const *iter2;
	gint g1;
	gint g2;
	GType type;

	iter1 = *i1;
	iter2 = *i2;

	// Sort on instruction type
	g1 = type_id (iter1->instruction);
	g2 = type_id (iter2->instruction);

	if (g1 != g2)
	{
		return g1 < g2 ? -1 : 1;
	}

	// Sort on depth
	if (iter1->depth != iter2->depth)
	{
		return iter1->depth > iter2->depth ? -1 : 1;
	}

	type = G_OBJECT_TYPE (iter1->instruction);

	if (type == CPG_TYPE_INSTRUCTION_FUNCTION ||
	    type == CPG_TYPE_INSTRUCTION_OPERATOR)
	{
		return compare_function (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION)
	{
		return compare_custom_function (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF)
	{
		return compare_custom_function_ref (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR)
	{
		return compare_custom_operator (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF)
	{
		return compare_custom_operator_ref (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_PROPERTY)
	{
		return compare_property (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_NUMBER ||
	         type == CPG_TYPE_INSTRUCTION_CONSTANT)
	{
		return compare_number (iter1, iter2);
	}

	return 0;
}

static gboolean
instruction_is_operator (CpgExpressionTreeIter *iter,
                         CpgMathOperatorType   *type)
{
	if (!CPG_IS_INSTRUCTION_OPERATOR (iter->instruction))
	{
		return FALSE;
	}

	*type = cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (iter->instruction));

	return TRUE;
}

static gboolean
instruction_is_multiply (CpgExpressionTreeIter *iter)
{
	CpgMathOperatorType type;

	return instruction_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_MULTIPLY;
}

static gboolean
instruction_is_divide (CpgExpressionTreeIter *iter)
{
	CpgMathOperatorType type;

	return instruction_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_DIVIDE;
}

static gboolean
instruction_is_plus (CpgExpressionTreeIter *iter)
{
	CpgMathOperatorType type;

	return instruction_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_PLUS;
}

static gboolean
instruction_is_minus (CpgExpressionTreeIter *iter)
{
	CpgMathOperatorType type;

	return instruction_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_MINUS;
}

static gboolean
instruction_is_unary_minus (CpgExpressionTreeIter *iter)
{
	CpgMathOperatorType type;

	return instruction_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_UNARY_MINUS;
}

typedef struct
{
	CpgExpressionTreeIter *iter;
	CpgExpressionTreeIter *parent;
	gint child_pos;
} CollectItem;

static CollectItem *
collect_item_new (CpgExpressionTreeIter *iter,
                  gint                   child_pos)
{
	CollectItem *ret;

	ret = g_slice_new (CollectItem);

	ret->iter = iter;
	ret->parent = iter->parent;
	ret->child_pos = child_pos;

	return ret;
}

static void
collect_item_free (CollectItem *item)
{
	g_slice_free (CollectItem, item);
}

static GSList *
collect_multiply (CpgExpressionTreeIter *iter,
                  GSList                *ret)
{
	gint i;

	for (i = 0; i < iter->num_children; ++i)
	{
		CpgExpressionTreeIter *c;

		c = iter->children[i];

		if (instruction_is_multiply (c))
		{
			// Go deep
			ret = collect_multiply (c, ret);
		}
		else if (instruction_is_divide (c))
		{
			CpgExpressionTreeIter *c1;

			c1 = c->children[0];

			if (instruction_is_multiply (c1))
			{
				ret = collect_multiply (c1, ret);
			}
			else
			{
				ret = g_slist_prepend (ret,
				                       collect_item_new (c1,
				                                         0));
			}
		}
		else
		{
			ret = g_slist_prepend (ret, collect_item_new (c, i));
		}
	}

	return ret;
}

static GSList *
collect_plus (CpgExpressionTreeIter *iter,
              GSList                *ret)
{
	gint i;

	for (i = 0; i < iter->num_children; ++i)
	{
		CpgExpressionTreeIter *c;

		c = iter->children[i];

		if (instruction_is_plus (c))
		{
			// Go deep
			ret = collect_plus (c, ret);
		}
		else
		{
			ret = g_slist_prepend (ret, collect_item_new (c, i));
		}
	}

	return ret;
}

static gint
compare_collect_item (CollectItem *i1,
                      CollectItem *i2)
{
	return compare_iters (&(i1->iter), &(i2->iter));
}

static void
swap_collect (GSList *ret)
{
	GSList *cp;
	GSList *cpitem;

	cp = g_slist_copy (ret);

	// Sort and then insert
	ret = g_slist_sort (ret, (GCompareFunc)compare_collect_item);

	cpitem = cp;

	while (ret)
	{
		CollectItem *item = ret->data;
		CollectItem *orig = cpitem->data;

		if (orig->iter != item->iter)
		{
			orig->parent->children[orig->child_pos] = item->iter;
			item->iter->parent = orig->parent;
		}

		ret = g_slist_next (ret);
		cpitem = g_slist_next (cpitem);
	}

	g_slist_free (cp);
}

static void
swap_multiply (CpgExpressionTreeIter *iter)
{
	GSList *ret;

	ret = g_slist_reverse (collect_multiply (iter, NULL));
	swap_collect (ret);

	g_slist_foreach (ret, (GFunc)collect_item_free, NULL);
	g_slist_free (ret);
}

static void
swap_plus (CpgExpressionTreeIter *iter)
{
	GSList *ret;

	ret = g_slist_reverse (collect_plus (iter, NULL));
	swap_collect (ret);

	g_slist_foreach (ret, (GFunc)collect_item_free, NULL);
	g_slist_free (ret);

}

static void
swap_operator (CpgExpressionTreeIter *iter)
{
	CpgMathOperatorType type;

	type = cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (iter->instruction));

	switch (type)
	{
		case CPG_MATH_OPERATOR_TYPE_MULTIPLY:
			swap_multiply (iter);
		break;
		case CPG_MATH_OPERATOR_TYPE_PLUS:
			swap_plus (iter);
		break;
		default:
		break;
	}
}

static void
canonical_unary_minus (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *one;
	CpgExpressionTreeIter *child;
	CpgInstruction *instr;

	instr = cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MULTIPLY,
	                                      "*",
	                                      2);

	cpg_mini_object_free (CPG_MINI_OBJECT (iter->instruction));
	iter->instruction = instr;

	child = iter->children[0];
	g_free (iter->children);

	one = iter_new (cpg_instruction_number_new (-1));

	iter->num_children = 2;
	iter->children = g_new (CpgExpressionTreeIter *, 2);

	iter->children[0] = one;
	one->parent = iter;

	iter->children[1] = child;
	child->parent = iter;
}

static void
canonical_minus (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *one;
	CpgExpressionTreeIter *mult;
	CpgInstruction *instr;

	// minus in canonical form is a plus of a -1 multiplied
	mult = iter_new (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MULTIPLY,
	                                               "*",
	                                               2));

	one = iter_new (cpg_instruction_number_new (-1));

	mult->num_children = 2;
	mult->children = g_new (CpgExpressionTreeIter *, 2);
	mult->children[0] = one;
	one->parent = mult;

	instr = cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
	                                      "+",
	                                      2);

	cpg_mini_object_free (CPG_MINI_OBJECT (iter->instruction));
	iter->instruction = instr;

	mult->children[1] = iter->children[1];
	mult->parent = iter;

	iter->children[1]->parent = mult;
	iter->children[1] = mult;
}

static gint
function_argument_index (CpgFunction *f,
                         CpgProperty *p)
{
	GList const *args;
	gint i = 0;

	args = cpg_function_get_arguments (f);

	while (args)
	{
		if (_cpg_function_argument_get_property (args->data) == p)
		{
			return i;
		}

		++i;
		args = g_list_next (args);
	}

	return -1;
}

static gint
iter_index_of (CpgExpressionTreeIter *parent,
               CpgExpressionTreeIter *iter)
{
	gint i;

	if (!parent)
	{
		if (!iter)
		{
			return -1;
		}

		parent = iter->parent;
	}

	for (i = 0; i < parent->num_children; ++i)
	{
		if (parent->children[i] == iter)
		{
			return i;
		}
	}

	return -1;
}

static void
replace_iter (CpgExpressionTreeIter *iter,
              CpgExpressionTreeIter *other)
{
	gint i;

	if (other)
	{
		replace_iter (other, NULL);
		other->parent = NULL;
	}

	if (!iter->parent)
	{
		return;
	}

	i = iter_index_of (NULL, iter);

	if (i == -1)
	{
		return;
	}

	iter->parent->children[i] = other;

	if (other != NULL)
	{
		other->parent = iter->parent;
	}

	iter->parent = NULL;
}

static void
copy_iter_into (CpgExpressionTreeIter *cp,
                CpgExpressionTreeIter *dest)
{
	gint i;

	for (i = 0; i < dest->num_children; ++i)
	{
		cpg_expression_tree_iter_free (dest->children[i]);
	}

	g_free (dest->children);

	dest->num_children = cp->num_children;
	dest->children = g_new (CpgExpressionTreeIter *, cp->num_children);

	for (i = 0; i < cp->num_children; ++i)
	{
		dest->children[i] = cp->children[i];

		cp->children[i]->parent = dest;
		cp->children[i] = NULL;
	}

	cpg_mini_object_free (CPG_MINI_OBJECT (dest->instruction));

	dest->instruction = cp->instruction;
	cp->instruction = NULL;
}

static void
canonical_custom_function_real (CpgExpressionTreeIter *iter,
                                CpgFunction           *f)
{
	CpgExpressionTreeIter *func;
	GQueue q;
	gint i;

	func = cpg_expression_tree_iter_new (cpg_function_get_expression (f));
	cpg_expression_tree_iter_canonicalize (func);

	// Replace all property instructions with the nodes which are the
	// current children of the iter
	g_queue_init (&q);

	g_queue_push_head (&q, func);

	while (!g_queue_is_empty (&q))
	{
		CpgExpressionTreeIter *it;

		it = g_queue_pop_head (&q);

		if (CPG_IS_INSTRUCTION_PROPERTY (it->instruction))
		{
			CpgProperty *prop;
			CpgInstructionProperty *pi;

			pi = CPG_INSTRUCTION_PROPERTY (it->instruction);
			prop = cpg_instruction_property_get_property (pi);

			if (cpg_property_get_object (prop) == CPG_OBJECT (f))
			{
				// Find argument index
				gint idx = function_argument_index (f, prop);
				CpgExpressionTreeIter *cp;

				cp = cpg_expression_tree_iter_copy (iter->children[idx]);

				if (it == func)
				{
					func = cp;
				}

				// Replace the iter with child arg
				replace_iter (it, cp);
				cpg_expression_tree_iter_free (it);
			}
		}
		else
		{
			for (i = 0; i < it->num_children; ++i)
			{
				g_queue_push_head (&q, it->children[i]);
			}
		}
	}

	copy_iter_into (func, iter);
	cpg_expression_tree_iter_free (func);
}

static void
canonical_custom_function (CpgExpressionTreeIter *iter)
{
	// Custom functions are flattened in canonical form
	CpgInstructionCustomFunction *instr;

	instr = CPG_INSTRUCTION_CUSTOM_FUNCTION (iter->instruction);
	canonical_custom_function_real (iter, cpg_instruction_custom_function_get_function (instr));
}

static void
canonical_custom_operator (CpgExpressionTreeIter *iter)
{
	// Custom operators that support funtions are flattened in canonical form
	CpgInstructionCustomOperator *instr;
	CpgOperator *op;
	CpgFunction *f;

	instr = CPG_INSTRUCTION_CUSTOM_OPERATOR (iter->instruction);
	op = cpg_instruction_custom_operator_get_operator (instr);

	if (CPG_IS_OPERATOR_DF_DT (op))
	{
		CpgExpressionTreeIter *cp;
		CpgOperatorDfDt *dfdt = CPG_OPERATOR_DF_DT (op);

		cp = cpg_expression_tree_iter_new (cpg_operator_df_dt_get_derived (dfdt));
		cpg_expression_tree_iter_canonicalize (cp);

		// Replace iter with cp
		copy_iter_into (cp, iter);
		cpg_expression_tree_iter_free (cp);
	}
	else
	{
		f = cpg_operator_get_primary_function (op);

		if (f)
		{
			canonical_custom_function_real (iter, f);
		}
	}
}

void
cpg_expression_tree_iter_canonicalize (CpgExpressionTreeIter *iter)
{
	gint i;

	// Canonicalize the children first
	for (i = 0; i < iter->num_children; ++i)
	{
		cpg_expression_tree_iter_canonicalize (iter->children[i]);
	}

	if (instruction_is_minus (iter))
	{
		canonical_minus (iter);
	}
	else if (instruction_is_unary_minus (iter))
	{
		canonical_unary_minus (iter);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (iter->instruction))
	{
		canonical_custom_function (iter);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (iter->instruction))
	{
		canonical_custom_operator (iter);
	}

	// Sort children when instruction is commutative
	if (cpg_instruction_get_is_commutative (iter->instruction))
	{
		qsort (iter->children,
		       iter->num_children,
		       sizeof (CpgExpressionTreeIter *),
		       (GCompareFunc)compare_iters);

		// Implement basic swapping of some operators
		if (CPG_IS_INSTRUCTION_OPERATOR (iter->instruction))
		{
			swap_operator (iter);
		}
	}
}

static CpgExpressionTreeIter *
iter_brother (CpgExpressionTreeIter *iter)
{
	gint i;

	for (i = 0; i < iter->parent->num_children; ++i)
	{
		if (iter->parent->children[i] != iter)
		{
			return iter->parent->children[i];
		}
	}

	return NULL;
}

static gboolean
instruction_is_number (CpgExpressionTreeIter *iter,
                       gdouble               *num)
{
	if (CPG_IS_INSTRUCTION_NUMBER (iter->instruction))
	{
		if (num)
		{
			*num = cpg_instruction_number_get_value (CPG_INSTRUCTION_NUMBER (iter->instruction));
		}

		return TRUE;
	}

	return FALSE;
}

static GSList *
find_premultiply (CpgExpressionTreeIter *iter,
                  GSList                *targets)
{
	if (instruction_is_number (iter, NULL))
	{
		targets = g_slist_prepend (targets, iter);
	}
	else if (instruction_is_multiply (iter))
	{
		GSList *ret;

		ret = find_premultiply (iter->children[0],
		                        targets);

		if (ret == targets)
		{
			ret = find_premultiply (iter->children[1],
			                        targets);
		}

		targets = ret;
	}
	else if (instruction_is_plus (iter) ||
	         instruction_is_minus (iter))
	{
		GSList *left;

		// Have to have numbers in both branches
		left = find_premultiply (iter->children[0], NULL);

		if (left)
		{
			GSList *right;

			right = find_premultiply (iter->children[1], NULL);

			if (right)
			{
				targets = g_slist_concat (right,
				                          g_slist_concat (left,
				                                          targets));
			}
			else
			{
				g_slist_free (left);
			}
		}
	}

	return targets;
}

static gboolean
cmp_double (gdouble a, gdouble b)
{
	return fabs (a - b) < 10e-9;
}

static CpgExpressionTreeIter *
simplify_function (CpgExpressionTreeIter *iter)
{
	CpgInstructionFunction *f;
	gint id;
	gboolean ret = TRUE;
	CpgExpressionTreeIter *retval = iter;
	gint i;
	CpgStack stack;

	f = CPG_INSTRUCTION_FUNCTION (iter->instruction);
	id = cpg_instruction_function_get_id (f);

	if (CPG_IS_INSTRUCTION_OPERATOR (f))
	{
		if (cpg_math_operator_is_variable (id))
		{
			return iter;
		}
	}
	else
	{
		if (cpg_math_function_is_variable (id))
		{
			return iter;
		}
	}

	cpg_stack_init (&stack, iter->num_children + 1);

	// Check if all arguments are numeric
	for (i = 0; i < iter->num_children; ++i)
	{
		gdouble num;

		if (!instruction_is_number (iter->children[i], &num))
		{
			ret = FALSE;
			break;
		}

		cpg_stack_push (&stack, num);
	}

	if (ret)
	{
		if (CPG_IS_INSTRUCTION_OPERATOR (f))
		{
			cpg_math_operator_execute (id,
			                           iter->num_children,
			                           &stack);
		}
		else
		{
			cpg_math_function_execute (id,
			                           iter->num_children,
			                           &stack);
		}

		retval = iter_new (cpg_instruction_number_new (cpg_stack_pop (&stack)));

		replace_iter (iter, retval);
		cpg_expression_tree_iter_free (iter);
	}

	cpg_stack_destroy (&stack);
	return retval;
}

static CpgExpressionTreeIter *
simplify_premultiply (CpgExpressionTreeIter *iter)
{
	GSList *left;
	GSList *right;
	gdouble num1;
	gdouble num2;
	gboolean isnum1;
	gboolean isnum2;
	CpgExpressionTreeIter *ret;

	ret = simplify_function (iter);

	if (ret != iter)
	{
		return ret;
	}

	isnum1 = instruction_is_number (iter->children[0], &num1);
	isnum2 = instruction_is_number (iter->children[1], &num2);

	if ((isnum1 && cmp_double (num1, 0)) || (isnum2 && cmp_double (num2, 0)))
	{
		// Eliminate node
		gint idx = 0;

		if (!(isnum1 && cmp_double (num1, 0)))
		{
			idx = 1;
		}

		ret = iter->children[idx];
		replace_iter (iter, ret);
		cpg_expression_tree_iter_free (iter);

		return ret;
	}
	else if (isnum1 && cmp_double (num1, 1))
	{
		// Replace
		ret = iter->children[1];
		replace_iter (iter, ret);
		cpg_expression_tree_iter_free (iter);

		return ret;
	}
	else if (isnum2 && cmp_double (num2, 1))
	{
		// Replace
		ret = iter->children[0];

		replace_iter (iter, ret);
		cpg_expression_tree_iter_free (iter);
		return ret;
	}

	left = find_premultiply (iter->children[0], NULL);
	right = find_premultiply (iter->children[1], NULL);

	ret = iter;

	if (left && right && (!left->next || !right->next))
	{
		CpgExpressionTreeIter *liter;
		CpgExpressionTreeIter *brother;
		GSList *item;

		// Premultiply from left to right
		if (!right->next && left->next)
		{
			GSList *tmp = left;
			left = right;
			right = tmp;
		}

		liter = left->data;
		num1 = cpg_instruction_number_get_value (CPG_INSTRUCTION_NUMBER (liter->instruction));

		for (item = right; item; item = g_slist_next (item))
		{
			CpgExpressionTreeIter *riter;
			CpgInstructionNumber *num;

			riter = item->data;
			num = CPG_INSTRUCTION_NUMBER (riter->instruction);

			num2 = cpg_instruction_number_get_value (num);

			cpg_instruction_number_set_value (num,
			                                  num1 * num2);

			if (riter->parent != liter->parent)
			{
				iter_simplify (riter->parent, FALSE);
			}
		}

		// Remove the left hand side
		brother = iter_brother (liter);

		if (liter->parent == iter)
		{
			ret = brother;
		}

		replace_iter (liter->parent, brother);

		cpg_expression_tree_iter_free (liter->parent);
		g_slist_free (right);
	}

	g_slist_free (left);
	return ret;
}

static CpgExpressionTreeIter *
find_preadd (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *ret = NULL;

	if (instruction_is_number (iter, NULL))
	{
		return iter;
	}
	else if (instruction_is_plus (iter))
	{
		ret = find_preadd (iter->children[0]);

		if (!ret)
		{
			ret = find_preadd (iter->children[1]);
		}
	}

	return ret;
}

static CpgExpressionTreeIter *
simplify_preadd (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *ret;
	CpgExpressionTreeIter *left;
	CpgExpressionTreeIter *right;

	ret = simplify_function (iter);

	if (ret != iter)
	{
		return ret;
	}

	left = find_preadd (iter->children[0]);
	right = find_preadd (iter->children[1]);

	if (left && right)
	{
		CpgInstructionNumber *nl;
		CpgInstructionNumber *nr;
		CpgExpressionTreeIter *brother;

		nl = CPG_INSTRUCTION_NUMBER (left->instruction);
		nr = CPG_INSTRUCTION_NUMBER (right->instruction);

		cpg_instruction_number_set_value (nr,
		                                  cpg_instruction_number_get_value (nr) +
		                                  cpg_instruction_number_get_value (nl));

		// Going to add them together
		// Remove the left hand side
		brother = iter_brother (left);

		if (left->parent == iter)
		{
			ret = brother;
		}

		replace_iter (left->parent, brother);
		cpg_expression_tree_iter_free (left->parent);
	}

	// See if the left part and the right part are equal
	if (cpg_expression_tree_iter_equal (ret->children[0], ret->children[1]))
	{
		CpgExpressionTreeIter *nt;
		CpgExpressionTreeIter *two;

		nt = iter_new (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MULTIPLY,
		                                             "*",
		                                             2));

		two = iter_new (cpg_instruction_number_new (2));

		nt->num_children = 2;
		nt->children = g_new (CpgExpressionTreeIter *, 2);

		nt->children[0] = two;
		two->parent = nt;

		nt->children[1] = ret->children[0];
		ret->children[0]->parent = nt;

		ret->children[0] = NULL;

		replace_iter (ret, nt);
		cpg_expression_tree_iter_free (ret);
		ret = nt;
	}

	return ret;
}

static GSList *
find_multiply_powers (CpgExpressionTreeIter *iter,
                      GSList                *ret)
{
	if (instruction_is_multiply (iter))
	{
		// Go deep, left to right
		gint i;

		for (i = 0; i < iter->num_children; ++i)
		{
			ret = find_multiply_powers (iter->children[i], ret);
		}
	}
	else
	{
		ret = g_slist_prepend (ret, iter);
	}

	return ret;
}

static CpgExpressionTreeIter *
power_up (CpgExpressionTreeIter *root,
          GSList                *from,
          GSList                *to)
{
	CpgExpressionTreeIter *pow;
	gchar *s;
	gint num;

	pow = iter_new (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_POWER,
	                                              "**",
	                                              2));

	pow->num_children = 2;
	pow->children = g_new0 (CpgExpressionTreeIter *, 2);

	replace_iter (from->data, pow);

	pow->children[0] = from->data;
	pow->children[0]->parent = pow;

	from = from->next;
	num = 1;

	while (from != to)
	{
		CpgExpressionTreeIter *it;

		it = iter_new (cpg_instruction_number_new_from_string ("1"));

		replace_iter (from->data, it);
		cpg_expression_tree_iter_free (from->data);

		from = g_slist_next (from);

		++num;
	}

	s = g_strdup_printf ("%d", num);

	pow->children[1] = iter_new (cpg_instruction_number_new_from_string (s));

	pow->children[1]->parent = pow;
	g_free (s);

	return root;
}

static CpgExpressionTreeIter *
simplify_multiply_powers (CpgExpressionTreeIter *iter)
{
	GSList *ret;
	GSList *start;
	GSList *item;
	gboolean didit = FALSE;

	ret = find_multiply_powers (iter, NULL);

	start = ret;
	item = g_slist_next (start);

	while (item)
	{
		gboolean eq;

		eq = cpg_expression_tree_iter_equal (start->data, item->data);

		if (start && start->next && start->next != item && !eq)
		{
			// We group from start until (but not including) item
			iter = power_up (iter, start, item);
			didit = TRUE;
		}

		if (!eq)
		{
			start = item;
		}

		item = g_slist_next (item);
	}

	if (start && start->next && cpg_expression_tree_iter_equal (start->data, start->next->data))
	{
		iter = power_up (iter, start, NULL);
		didit = TRUE;
	}

	if (didit)
	{
		// Recurse
		iter = cpg_expression_tree_iter_simplify (iter);
	}

	g_slist_free (ret);
	return iter;
}

static CpgExpressionTreeIter *
simplify_predivide (CpgExpressionTreeIter *iter)
{
	gdouble num1;
	gdouble num2;
	gboolean isnum1;
	gboolean isnum2;
	CpgExpressionTreeIter *ret;

	ret = simplify_function (iter);

	if (ret != iter)
	{
		return ret;
	}

	isnum1 = instruction_is_number (iter->children[0], &num1);
	isnum2 = instruction_is_number (iter->children[1], &num2);

	if (isnum1 && cmp_double (num1, 0))
	{
		// Division of 0 by something is 0
		ret = iter->children[0];
		replace_iter (iter, ret);
		cpg_expression_tree_iter_free (iter);
	}
	else if (isnum2 && cmp_double (num2, 1))
	{
		// Division of something by 1 is something
		ret = iter->children[0];
		replace_iter (iter, ret);
		cpg_expression_tree_iter_free (iter);
	}
	else if (cpg_expression_tree_iter_equal (iter->children[0], iter->children[1]))
	{
		// Replace by '1'
		ret = iter_new (cpg_instruction_number_new_from_string ("1"));

		replace_iter (iter, ret);
		cpg_expression_tree_iter_free (iter);
	}

	return ret;
}

static CpgExpressionTreeIter *
iter_simplify (CpgExpressionTreeIter *iter,
               gboolean               simplify_children)
{
	if (simplify_children)
	{
		gint num;
		gint i;

		num = iter->num_children;

		// First simplify the children
		for (i = 0; i < num; ++i)
		{
			iter->children[i] = iter_simplify (iter->children[i],
			                                   TRUE);
		}
	}

	// Then simplify the iter itself
	if (instruction_is_multiply (iter))
	{
		// Try to premultiply
		iter = simplify_premultiply (iter);

		// Try to make powers
		iter = simplify_multiply_powers (iter);
	}
	else if (instruction_is_plus (iter))
	{
		// Try to preadd
		iter = simplify_preadd (iter);
	}
	else if (instruction_is_divide (iter))
	{
		iter = simplify_predivide (iter);
	}
	else if (CPG_IS_INSTRUCTION_FUNCTION (iter->instruction))
	{
		iter = simplify_function (iter);
	}

	return iter;
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_simplify (CpgExpressionTreeIter *iter)
{
	return iter_simplify (iter, TRUE);
}

gboolean
cpg_expression_tree_iter_equal (CpgExpressionTreeIter *iter,
                                CpgExpressionTreeIter *other)
{
	gint i;

	if (!cpg_instruction_equal (iter->instruction, other->instruction))
	{
		return FALSE;
	}

	if (iter->num_children != other->num_children)
	{
		return FALSE;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		if (!cpg_expression_tree_iter_equal (iter->children[i], other->children[i]))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static GSList *
find_properties (CpgExpressionTreeIter *iter,
                 CpgProperty           *prop,
                 GSList                *ret)
{
	gint i;

	for (i = 0; i < iter->num_children; ++i)
	{
		ret = find_properties (iter->children[i], prop, ret);
	}

	if (CPG_IS_INSTRUCTION_PROPERTY (iter->instruction))
	{
		CpgInstructionProperty *instr;

		instr = CPG_INSTRUCTION_PROPERTY (iter->instruction);

		if (cpg_instruction_property_get_property (instr) == prop)
		{
			ret = g_slist_prepend (ret, iter);
		}
	}

	return ret;
}

static CpgExpressionTreeIter *
make_coefficient (CpgExpressionTreeIter  *root,
                  CpgExpressionTreeIter  *child)
{
	CpgExpressionTreeIter *parent;
	gint idx;

	if (!child)
	{
		return NULL;
	}

	if (root == child)
	{
		cpg_expression_tree_iter_free (child);
		return iter_new (cpg_instruction_number_new_from_string ("1"));
	}

	parent = child->parent;
	idx = iter_index_of (parent, child);

	if (idx >= 0)
	{
		CpgExpressionTreeIter *brother;

		brother = parent->children[!idx];
		parent->children[!idx] = NULL;
		brother->parent = NULL;

		copy_iter_into (brother, parent);
	}

	return root;
}

static CpgExpressionTreeIter *
sum_terms (GSList *terms,
           GSList *ignore)
{
	CpgExpressionTreeIter *root;
	CpgExpressionTreeIter *cur;
	GSList *item;

	if (!terms || (!terms->next && terms == ignore))
	{
		return NULL;
	}

	if (!terms->next || (!terms->next->next && terms->next == ignore))
	{
		return terms->data;
	}

	if (!terms->next->next && terms == ignore)
	{
		return terms->next->data;
	}

	root = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
	                                                     "+",
	                                                     2),
	                       2);

	cur = root;
	cur->children[0] = terms->data;
	cur->children[0]->parent = cur;

	for (item = terms->next; item; item = g_slist_next (item))
	{
		if (item == ignore)
		{
			continue;
		}

		if (item->next)
		{
			CpgExpressionTreeIter *np;

			np = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
			                                                   "+",
			                                                   2),
			                     2);

			cur->children[1] = np;
			np->parent = cur;

			np->children[0] = item->data;
			np->children[0]->parent = np;

			cur = np;
		}
		else
		{
			cur->children[1] = item->data;
			cur->children[1]->parent = cur;
		}
	}

	return root;
}

static CpgExpressionTreeIter *
solve_coefficient (CpgExpressionTreeIter  *root,
                   CpgExpressionTreeIter  *child,
                   CpgExpressionTreeIter **coefficient,
                   GError                **error)
{
	CpgExpressionTreeIter *parent = child->parent;
	GSList *terms = NULL;
	CpgExpressionTreeIter *prev = child;
	GSList *myterm = NULL;

	while (parent)
	{
		cpg_debug_message (DEBUG_LINSOLVE,
		                   "Solving coefficient for: %s",
		                   cpg_expression_tree_iter_to_string (parent));

		if (instruction_is_plus (parent))
		{
			gboolean first = !terms;
			gint i;
			CpgExpressionTreeIter *tmp;

			for (i = 0; i < parent->num_children; ++i)
			{
				if (first || parent->children[i] != prev)
				{
					// Add another term
					terms = g_slist_prepend (terms,
					                         parent->children[i]);

					if (first && parent->children[i] == prev)
					{
						myterm = terms;
					}
				}

				if (parent->children[i])
				{
					parent->children[i]->parent = NULL;
					parent->children[i] = NULL;
				}
			}

			cpg_debug_message (DEBUG_LINSOLVE,
			                   "Added linear terms, new parent: %s",
			                   cpg_expression_tree_iter_to_string (parent->parent));

			tmp = parent;
			parent = parent->parent;
			replace_iter (tmp, NULL);
			prev = NULL;

			if (tmp == root)
			{
				root = NULL;
			}

			cpg_expression_tree_iter_free (tmp);
		}
		else
		{
			gboolean ismult = instruction_is_multiply (parent);
			gboolean isdiv = !ismult && instruction_is_divide (parent);

			gint idx = iter_index_of (parent, prev);

			if (!ismult && (!isdiv || idx == 1))
			{
				g_set_error (error,
				             CPG_SYMBOLIC_ERROR,
				             CPG_SYMBOLIC_ERROR_UNSUPPORTED,
				             "Expression `%s' for linear solve is not linear in `%s'",
				             cpg_expression_tree_iter_to_string (root),
				             cpg_expression_tree_iter_to_string (child));

				// TODO: cleanup
				return NULL;
			}

			// Replace ourselves (prev) in parent with NULL
			if (terms)
			{
				GSList *item;

				if (parent->children[idx])
				{
					parent->children[idx]->parent = NULL;
				}

				parent->children[idx] = NULL;

				// We need to apply this to our current terms
				for (item = terms; item; item = g_slist_next (item))
				{
					CpgExpressionTreeIter *cp;

					cp = cpg_expression_tree_iter_copy (parent);

					cp->children[idx] = item->data;
					cp->children[idx]->parent = cp;

					item->data = cp;
				}
			}

			prev = parent;
			parent = parent->parent;
		}
	}

	*coefficient = make_coefficient (myterm ? myterm->data : NULL, child);
	cpg_expression_tree_iter_free (root);

	if (!terms)
	{
		// nothing to separate, no plus operators where encountered
		return NULL;
	}

	root = sum_terms (terms, myterm);

	g_slist_free (terms);
	return root;
}

/**
 * cpg_expression_tree_iter_solve_for:
 * @iter: A #CpgExpressionTreeIter
 * @prop: A #CpgProperty
 *
 * NOTE: This is only supposed to work for canonical expressions linear in
 * @prop
 *
 * Returns: A #CpgExpressionTreeIter
 *
 **/
CpgExpressionTreeIter *
cpg_expression_tree_iter_solve_for (CpgExpressionTreeIter  *iter,
                                    CpgProperty            *prop,
                                    GError                **error)
{
	GSList *props;
	GSList *item;
	CpgExpressionTreeIter *ret;
	CpgExpressionTreeIter *inv;
	GError *err = NULL;
	gboolean retval = TRUE;
	GSList *coefs = NULL;
	CpgExpressionTreeIter *summed;
	CpgExpressionTreeIter *div;

	g_return_val_if_fail (CPG_IS_PROPERTY (prop), NULL);

	ret = cpg_expression_tree_iter_copy (iter);

	// We are going to do a solve iter for prop assuming F(iter) = 0
	props = find_properties (ret, prop, NULL);

	if (!props)
	{
		g_set_error (error,
		             CPG_SYMBOLIC_ERROR,
		             CPG_SYMBOLIC_ERROR_INVALID,
		             "Expression `%s' cannot be solved towards `%s'",
		             cpg_expression_tree_iter_to_string (iter),
		             cpg_property_get_name (prop));

		cpg_expression_tree_iter_free (ret);
		return NULL;
	}

	// Now factor out for each of the instances of prop
	for (item = props; item; item = g_slist_next (item))
	{
		CpgExpressionTreeIter *child = item->data;
		CpgExpressionTreeIter *coef = NULL;

		// Separate child (prop) from the expression, the result is
		// a new expression without the coefficient on prop
		ret = solve_coefficient (ret, child, &coef, &err);

		if (err)
		{
			g_propagate_error (error, err);
			retval = FALSE;
			break;
		}

		//g_message ("%s", cpg_expression_tree_iter_to_string (ret));

		cpg_debug_message (DEBUG_LINSOLVE, "Coefficient: `%s' (rest: `%s')",
		                   cpg_expression_tree_iter_to_string (coef),
		                   cpg_expression_tree_iter_to_string (ret));

		coefs = g_slist_prepend (coefs, coef);
	}

	g_slist_free (props);

	if (!retval)
	{
		g_slist_foreach (coefs, (GFunc)cpg_expression_tree_iter_free, NULL);
		g_slist_free (coefs);
		cpg_expression_tree_iter_free (ret);

		return NULL;
	}

	if (!ret)
	{
		// This means that there were only coefficients, and that means
		// no RHS, so return just 0
		g_slist_foreach (coefs, (GFunc)cpg_expression_tree_iter_free, NULL);
		g_slist_free (coefs);

		return iter_new (cpg_instruction_number_new_from_string ("0"));
	}

	// Now we have the expression 'ret' and a set of coefficients 'coefs'
	// We need to add the coefs together, negate it and divide the 'ret'
	// expression with it
	summed = cpg_expression_tree_iter_simplify (sum_terms (coefs, NULL));
	ret = cpg_expression_tree_iter_simplify (ret);

	inv = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
	                                                    "-",
	                                                    1),
	                      1);

	inv->children[0] = ret;
	inv->children[0]->parent = inv;

	// Now divide inv by summed
	div = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_DIVIDE,
	                                                    "/",
	                                                    2),
	                      2);

	div->children[0] = inv;
	div->children[0]->parent = div;

	div->children[1] = summed;
	div->children[1]->parent = div;

	return cpg_expression_tree_iter_simplify (div);
}
