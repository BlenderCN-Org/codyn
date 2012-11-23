#include "cdn-instruction-variable.h"

#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-edge-action.h>
#include <codyn/cdn-math.h>
#include <codyn/cdn-expression-tree-iter.h>
#include <codyn/cdn-compile-error.h>

#define CDN_INSTRUCTION_VARIABLE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_VARIABLE, CdnInstructionVariablePrivate))

struct _CdnInstructionVariablePrivate
{
	CdnVariable *property;
	CdnInstructionVariableBinding binding;
	CdnStackManipulation smanip;

	guint *slice;
	guint slice_length;
	CdnDimension slice_dim;
};

G_DEFINE_TYPE (CdnInstructionVariable, cdn_instruction_variable, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_variable_finalize (CdnMiniObject *object)
{
	CdnInstructionVariable *self;

	self = CDN_INSTRUCTION_VARIABLE (object);

	cdn_instruction_variable_set_variable (self, NULL);
	cdn_instruction_variable_set_slice (self, NULL, 0, NULL);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_variable_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_variable_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionVariable *self;
	CdnInstructionVariable const *src;

	src = CDN_INSTRUCTION_VARIABLE_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_variable_parent_class)->copy (object);

	self = CDN_INSTRUCTION_VARIABLE (ret);

	cdn_instruction_variable_set_variable (self, src->priv->property);
	self->priv->binding = src->priv->binding;

	cdn_instruction_variable_set_slice (self,
	                                    src->priv->slice,
	                                    src->priv->slice_length,
	                                    &src->priv->slice_dim);

	cdn_stack_manipulation_copy (&self->priv->smanip, &src->priv->smanip);

	return ret;
}

static gchar *
cdn_instruction_variable_to_string (CdnInstruction *instruction)
{
	CdnInstructionVariable *self;
	gchar *s;
	gchar *ret;

	self = CDN_INSTRUCTION_VARIABLE (instruction);
	s = cdn_variable_get_full_name_for_display (self->priv->property);

	if (self->priv->slice)
	{
		GString *st = g_string_new ("PRP");
		guint r;
		guint i = 0;

		g_string_append_printf (st, " (%s[", s);

		for (r = 0; r < self->priv->slice_dim.rows; ++r)
		{
			guint c;

			if (r != 0)
			{
				g_string_append (st, "; ");
			}

			for (c = 0; c < self->priv->slice_dim.columns; ++c)
			{
				if (c != 0)
				{
					g_string_append (st, ", ");
				}

				g_string_append_printf (st, "%u", self->priv->slice[i]);
				++i;
			}
		}

		g_string_append (st, "])");
		ret = g_string_free (st, FALSE);
	}
	else
	{
		ret = g_strdup_printf ("PRP (%s)", s);
	}

	g_free (s);

	return ret;
}

static void
cdn_instruction_variable_execute (CdnInstruction *instruction,
                                  CdnStack       *stack)
{
	CdnMatrix const *values;
	gdouble const *vals;

	CdnInstructionVariable *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionVariable *)instruction;

	values = cdn_variable_get_values (self->priv->property);
	vals = cdn_matrix_get (values);

	if (self->priv->slice)
	{
		guint i;

		for (i = 0; i < self->priv->slice_length; ++i)
		{
			cdn_stack_push (stack, vals[self->priv->slice[i]]);
		}
	}
	else
	{
		cdn_stack_pushn (stack,
		                 vals,
		                 cdn_matrix_size (values));
	}
}

static gboolean
variable_is_sparse (CdnVariable *v)
{
	// Cannot be sparse if it's marked as in
	if (cdn_variable_get_flags (v) & CDN_VARIABLE_FLAG_IN)
	{
		return FALSE;
	}

	return TRUE;
}

static guint *
map_slice_sparsity (CdnStackArg const *arg,
                    guint       const *slice,
                    guint              slice_length,
                    guint             *retlen)
{
	guint *ret;
	guint *rret;
	guint i;
	gint sidx = 0;

	if (arg->sparsity == NULL)
	{
		*retlen = 0;
		return NULL;
	}

	ret = g_new0 (guint, slice_length);

	for (i = 0; i < slice_length; ++i)
	{
		if (cdn_stack_arg_is_sparse (arg, slice[i]))
		{
			ret[sidx] = i;
			++sidx;
		}
	}

	*retlen = sidx;
	rret = g_memdup (ret, sizeof (guint) * sidx);
	g_free (ret);

	return rret;
}

static CdnStackManipulation const *
cdn_instruction_variable_get_stack_manipulation (CdnInstruction  *instruction,
                                                 GError         **error)
{
	CdnInstructionVariable *self;

	self = (CdnInstructionVariable *)instruction;

	if (self->priv->property)
	{
		CdnExpression *expr;
		CdnStackArg const *arg;

		expr = cdn_variable_get_expression (self->priv->property);

		if (!cdn_expression_get_dimension (expr,
		                                   &self->priv->smanip.push.dimension))
		{
			gchar *name;

			name = cdn_variable_get_full_name_for_display (self->priv->property);

			g_set_error (error,
			             CDN_COMPILE_ERROR_TYPE,
			             CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			             "Could not statically determine dimensionality of `%s'",
			             name);

			g_free (name);
			return NULL;
		}

		if (self->priv->slice)
		{
			self->priv->smanip.push.dimension = self->priv->slice_dim;
		}

		if (variable_is_sparse (self->priv->property))
		{
			CdnExpression *e;

			e = cdn_variable_get_expression (self->priv->property);

			// Check to see how to compute sparsity...
			arg = cdn_expression_get_stack_arg (e);

			if (arg->num_sparse > 0)
			{
				if (self->priv->slice)
				{
					guint *d;
					guint retlen;

					d = map_slice_sparsity (arg,
					                        self->priv->slice,
					                        self->priv->slice_length,
					                        &retlen);

					cdn_stack_arg_set_sparsity (&self->priv->smanip.push,
					                            d,
					                            retlen);

					g_free (d);
				}
				else
				{
					cdn_stack_arg_set_sparsity (&self->priv->smanip.push,
					                            arg->sparsity,
					                            arg->num_sparse);
				}
			}
		}
	}

	return &self->priv->smanip;
}

static GSList *
cdn_instruction_variable_get_dependencies (CdnInstruction *instruction)
{
	return g_slist_prepend (NULL, cdn_variable_get_expression (CDN_INSTRUCTION_VARIABLE (instruction)->priv->property));
}

static gboolean
slice_equal (CdnInstructionVariable *p1,
             CdnInstructionVariable *p2)
{
	guint i;

	if ((p1->priv->slice != NULL) != (p2->priv->property != NULL))
	{
		return FALSE;
	}

	if (p1->priv->slice == NULL)
	{
		return TRUE;
	}

	if (!cdn_dimension_equal (&p1->priv->slice_dim,
	                          &p2->priv->slice_dim))
	{
		return FALSE;
	}

	for (i = 0; i < p1->priv->slice_length; ++i)
	{
		if (p1->priv->slice[i] != p2->priv->slice[i])
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gboolean
cdn_instruction_variable_equal (CdnInstruction *i1,
                                CdnInstruction *i2,
                                gboolean        asstring)
{
	CdnInstructionVariable *p1 = CDN_INSTRUCTION_VARIABLE (i1);
	CdnInstructionVariable *p2 = CDN_INSTRUCTION_VARIABLE (i2);

	if (asstring)
	{
		return g_strcmp0 (cdn_variable_get_name (p1->priv->property),
		                  cdn_variable_get_name (p2->priv->property)) == 0 &&
		       p1->priv->binding == p2->priv->binding;
	}
	else
	{
		return p1->priv->property == p2->priv->property && slice_equal (p1, p2);
	}
}

static void
cdn_instruction_variable_class_init (CdnInstructionVariableClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_variable_finalize;
	object_class->copy = cdn_instruction_variable_copy;

	inst_class->to_string = cdn_instruction_variable_to_string;
	inst_class->execute = cdn_instruction_variable_execute;
	inst_class->get_stack_manipulation = cdn_instruction_variable_get_stack_manipulation;
	inst_class->get_dependencies = cdn_instruction_variable_get_dependencies;
	inst_class->equal = cdn_instruction_variable_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionVariablePrivate));
}

static void
cdn_instruction_variable_init (CdnInstructionVariable *self)
{
	self->priv = CDN_INSTRUCTION_VARIABLE_GET_PRIVATE (self);
}

/**
 * cdn_instruction_variable_new_with_binding:
 * @property: (transfer none): A #CdnVariable
 * @binding: A #CdnInstructionVariableBinding
 *
 * Create a new #CdnInstructionVariable.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_variable_new_with_binding (CdnVariable                   *property,
                                           CdnInstructionVariableBinding  binding)
{
	CdnMiniObject *ret;
	CdnInstructionVariable *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_VARIABLE);
	self = CDN_INSTRUCTION_VARIABLE (ret);

	cdn_instruction_variable_set_variable (self, property);
	self->priv->binding = binding;

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_variable_new:
 * @property: (transfer none): A #CdnVariable
 *
 * Create a new #CdnInstructionVariable.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_variable_new (CdnVariable *property)
{
	return cdn_instruction_variable_new_with_binding (property,
	                                                  CDN_INSTRUCTION_VARIABLE_BINDING_NONE);
}

/**
 * cdn_instruction_variable_set_variable:
 * @instruction: A #CdnInstructionVariable
 * @property: (transfer none): A #CdnVariable
 *
 * Set the property executed by the instruction.
 *
 **/
void
cdn_instruction_variable_set_variable (CdnInstructionVariable *instruction,
                                       CdnVariable            *property)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction));

	if (instruction->priv->property)
	{
		cdn_usable_unuse (CDN_USABLE (instruction->priv->property));
		g_object_unref (instruction->priv->property);

		instruction->priv->property = NULL;
	}

	if (property)
	{
		instruction->priv->property = g_object_ref_sink (property);
		cdn_usable_use (CDN_USABLE (instruction->priv->property));
	}
}

/**
 * cdn_instruction_variable_get_variable:
 * @instruction: A #CdnInstructionVariable
 *
 * Get the property executed by the instruction.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_instruction_variable_get_variable (CdnInstructionVariable *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction), NULL);
	return instruction->priv->property;
}

void
cdn_instruction_variable_set_binding (CdnInstructionVariable        *instruction,
                                      CdnInstructionVariableBinding  binding)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction));

	instruction->priv->binding = binding;
}

CdnInstructionVariableBinding
cdn_instruction_variable_get_binding (CdnInstructionVariable *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction),
	                      CDN_INSTRUCTION_VARIABLE_BINDING_NONE);

	return instruction->priv->binding;
}

void
cdn_instruction_variable_apply_slice (CdnInstructionVariable *instruction,
                                      guint const            *slice,
                                      guint                   length,
                                      CdnDimension const     *dim)
{
	guint *ns;
	guint i;

	g_return_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction));
	g_return_if_fail (slice == NULL || (dim != NULL && cdn_dimension_size (dim) == length));

	if (instruction->priv->slice == NULL)
	{
		cdn_instruction_variable_set_slice (instruction,
		                                    slice,
		                                    length,
		                                    dim);

		return;
	}

	// Compute slice on slice
	ns = g_memdup (slice, sizeof (guint) * length);

	for (i = 0; i < length; ++i)
	{
		ns[i] = instruction->priv->slice[slice[i]];
	}

	instruction->priv->slice_length = length;
	instruction->priv->slice = ns;
	instruction->priv->slice_dim = *dim;
}

void
cdn_instruction_variable_set_slice (CdnInstructionVariable *instruction,
                                    guint const            *slice,
                                    guint                   length,
                                    CdnDimension const     *dim)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction));
	g_return_if_fail (slice == NULL || (dim != NULL && cdn_dimension_size (dim) == length));

	g_free (instruction->priv->slice);

	instruction->priv->slice_length = length;

	if (slice)
	{
		instruction->priv->slice = g_memdup (slice, sizeof (guint) * length);
		instruction->priv->slice_dim = *dim;
	}
	else
	{
		instruction->priv->slice = NULL;
		instruction->priv->slice_dim.rows = 0;
		instruction->priv->slice_dim.columns = 0;
	}
}

/**
 * cdn_instruction_variable_get_slice:
 * @instruction: a #CdnInstructionVariable.
 * @length: (out): the length of the returned slice array.
 * @dim: (out): the dimension of the slice.
 *
 * Get the slice, if any, of this variable.
 *
 * Returns: (array length=length): the slice indices.
 *
 **/
guint const *
cdn_instruction_variable_get_slice (CdnInstructionVariable *instruction,
                                    guint                  *length,
                                    CdnDimension           *dim)
{
	if (length)
	{
		*length = 0;
	}

	if (dim)
	{
		dim->rows = 0;
		dim->columns = 0;
	}

	g_return_val_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction), NULL);

	if (length)
	{
		*length = instruction->priv->slice_length;
	}

	if (dim)
	{
		*dim = instruction->priv->slice_dim;
	}

	return instruction->priv->slice;
}

/**
 * cdn_instruction_variable_has_slice:
 * @instruction: a #CdnInstructionVariable.
 *
 * Get whether this variable instruction has a slice associated with it.
 *
 * Returns: %TRUE if the instruction is sliced, %FALSE otherwise.
 *
 **/
gboolean
cdn_instruction_variable_has_slice (CdnInstructionVariable *instruction)
{
	return instruction->priv->slice != NULL;
}
