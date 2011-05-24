#include "cpg-selection.h"
#include "cpg-expansion.h"

#define CPG_SELECTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTION, CpgSelectionPrivate))

struct _CpgSelectionPrivate
{
	gpointer  object;
	GSList   *expansions;
};

G_DEFINE_TYPE (CpgSelection, cpg_selection, G_TYPE_OBJECT)

static void
cpg_selection_finalize (GObject *object)
{
	CpgSelection *selection;

	selection = CPG_SELECTION (object);

	if (selection->priv->object)
	{
		g_object_unref (selection->priv->object);
	}

	g_slist_foreach (selection->priv->expansions, (GFunc)g_object_unref, NULL);
	g_slist_free (selection->priv->expansions);

	G_OBJECT_CLASS (cpg_selection_parent_class)->finalize (object);
}

static void
cpg_selection_class_init (CpgSelectionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_selection_finalize;

	g_type_class_add_private (object_class, sizeof (CpgSelectionPrivate));
}

static void
cpg_selection_init (CpgSelection *self)
{
	self->priv = CPG_SELECTION_GET_PRIVATE (self);
}

static GSList *
copy_expansions (GSList *list)
{
	GSList *ret = NULL;

	while (list)
	{
		ret = g_slist_prepend (ret,
		                       cpg_expansion_copy (list->data));

		list = g_slist_next (list);
	}

	return g_slist_reverse (ret);
}

CpgSelection *
cpg_selection_new (gpointer  object,
                   GSList   *expansions)
{
	CpgSelection *ret;

	ret = g_object_new (CPG_TYPE_SELECTION,
	                    NULL);

	ret->priv->object = object ? g_object_ref (object) : NULL;
	ret->priv->expansions = copy_expansions (expansions);

	return ret;
}

CpgSelection *
cpg_selection_copy (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return cpg_selection_new (selection->priv->object,
	                          selection->priv->expansions);
}

gpointer
cpg_selection_get_object (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return selection->priv->object;
}

GSList *
cpg_selection_get_expansions (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return selection->priv->expansions;
}
