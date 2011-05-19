#include "cpg-selection.h"
#include "cpg-expansion.h"

struct _CpgSelection
{
	gpointer  object;
	GSList   *expansions;
};

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

	ret = g_slice_new0 (CpgSelection);

	ret->object = g_object_ref (object);
	ret->expansions = copy_expansions (expansions);

	return ret;
}

CpgSelection *
cpg_selection_copy (CpgSelection *selection)
{
	return cpg_selection_new (selection->object, selection->expansions);
}

void
cpg_selection_free (CpgSelection *selection)
{
	g_object_unref (selection->object);

	g_slist_foreach (selection->expansions, (GFunc)cpg_expansion_free, NULL);
	g_slist_free (selection->expansions);

	g_slice_free (CpgSelection, selection);
}

CpgObject *
cpg_selection_get_object (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_OBJECT (selection->object), NULL);

	return CPG_OBJECT (selection->object);
}

CpgProperty *
cpg_selection_get_property (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (selection->object), NULL);

	return CPG_PROPERTY (selection->object);
}

GSList *
cpg_selection_get_expansions (CpgSelection *selection)
{
	return selection->expansions;
}
