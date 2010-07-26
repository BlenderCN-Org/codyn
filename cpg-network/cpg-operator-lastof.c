#include "cpg-operator-lastof.h"
#include "cpg-operator.h"
#include "cpg-instructions.h"
#include "cpg-property.h"

#define CPG_OPERATOR_LASTOF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_LASTOF, CpgOperatorLastofPrivate))

struct _CpgOperatorLastofPrivate
{
};

typedef struct
{
	CpgInstructionNumber *instruction;
	CpgProperty *property;
} PropertyData;

typedef struct
{
	CpgOperatorData parent;

	GSList *properties;
} CpgOperatorLastofData;

static void cpg_operator_lastof_iface_init (CpgOperatorInterface *iface);

G_DEFINE_TYPE_EXTENDED (CpgOperatorLastof,
                        cpg_operator_lastof,
                        G_TYPE_OBJECT,
                        0,
                        G_IMPLEMENT_INTERFACE (CPG_TYPE_OPERATOR,
                                               cpg_operator_lastof_iface_init));

static gchar *
cpg_operator_lastof_get_name (CpgOperator *op)
{
	return g_strdup ("lastof");
}

static void
scan_properties (CpgOperatorLastofData *data)
{
	CpgExpression *expression = data->parent.expressions->data;

	/* Replace all property instances with a number instruction and record
	   it */

	GSList const *instructions;
	GSList *new_instructions = NULL;

	instructions = cpg_expression_get_instructions (expression);

	while (instructions)
	{
		CpgInstruction *inst = CPG_INSTRUCTION (instructions->data);
		CpgInstruction *newinst;

		if (inst->type == CPG_INSTRUCTION_TYPE_PROPERTY)
		{
			CpgInstructionProperty *prop =
				CPG_INSTRUCTION_PROPERTY (inst);

			PropertyData *propdata = g_slice_new (PropertyData);

			newinst = cpg_instruction_number_new (0);

			_cpg_property_use (prop->property);

			propdata->property = g_object_ref (prop->property);
			propdata->instruction = CPG_INSTRUCTION_NUMBER (newinst);

			data->properties = g_slist_prepend (data->properties,
			                                    propdata);
		}
		else
		{
			newinst = cpg_instruction_copy (inst);
		}

		new_instructions = g_slist_prepend (new_instructions, newinst);
		instructions = g_slist_next (instructions);
	}

	new_instructions = g_slist_reverse (new_instructions);
	data->properties = g_slist_reverse (data->properties);

	cpg_expression_set_instructions (expression, new_instructions);
}

static CpgOperatorData *
cpg_operator_lastof_create_data (CpgOperator *op,
                                 GSList      *expressions)
{
	CpgOperatorLastofData *data;

	data = cpg_operator_data_new (CpgOperatorLastofData, expressions);

	scan_properties (data);

	return (CpgOperatorData *)data;
}

static void
property_data_free (PropertyData *data)
{
	_cpg_property_unuse (data->property);
	g_object_unref (data->property);
}

static void
cpg_operator_lastof_free_data (CpgOperator     *op,
                               CpgOperatorData *data)
{
	CpgOperatorLastofData *ldata = (CpgOperatorLastofData *)data;

	cpg_operator_data_destroy (data);

	g_slist_foreach (ldata->properties, (GFunc)property_data_free, NULL);
	g_slist_free (ldata->properties);

	g_slice_free (CpgOperatorLastofData, ldata);
}

static void
transfer_values (CpgOperatorLastofData *data)
{
	GSList *item;

	for (item = data->properties; item; item = g_slist_next (item))
	{
		PropertyData *d = item->data;

		d->instruction->value = cpg_property_get_last_value (d->property);
	}
}

static void
cpg_operator_lastof_execute (CpgOperator     *op,
                             CpgOperatorData *data,
                             CpgStack        *stack)
{
	transfer_values ((CpgOperatorLastofData *)data);

	cpg_stack_push (stack,
	                cpg_expression_evaluate (data->expressions->data));
}

static void
cpg_operator_lastof_iface_init (CpgOperatorInterface *iface)
{
	iface->get_name = cpg_operator_lastof_get_name;
	iface->create_data = cpg_operator_lastof_create_data;
	iface->free_data = cpg_operator_lastof_free_data;
	iface->execute = cpg_operator_lastof_execute;
}

static void
cpg_operator_lastof_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_operator_lastof_parent_class)->finalize (object);
}

static void
cpg_operator_lastof_class_init (CpgOperatorLastofClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_operator_lastof_finalize;

	/*g_type_class_add_private (object_class, sizeof(CpgOperatorLastofPrivate));*/
}

static void
cpg_operator_lastof_init (CpgOperatorLastof *self)
{
	/*self->priv = CPG_OPERATOR_LASTOF_GET_PRIVATE (self);*/
}

CpgOperatorLastof *
cpg_operator_lastof_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_LASTOF, NULL);
}
