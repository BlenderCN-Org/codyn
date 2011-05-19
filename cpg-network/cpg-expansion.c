#include "cpg-expansion.h"

struct _CpgExpansion
{
	GPtrArray *expansions;
};

CpgExpansion *
cpg_expansion_new_one (gchar const *item)
{
	gchar const *items[] = {
		item,
		NULL
	};

	return cpg_expansion_new (items);
}

CpgExpansion *
cpg_expansion_new (gchar const * const *items)
{
	CpgExpansion *ret;

	ret = g_slice_new (CpgExpansion);

	ret->expansions = g_ptr_array_new ();

	while (items && *items)
	{
		g_ptr_array_add (ret->expansions, g_strdup (*items));
		++items;
	}

	if (ret->expansions->len == 0)
	{
		g_ptr_array_add (ret->expansions, g_strdup (""));
	}

	return ret;
}

gint
cpg_expansion_num (CpgExpansion *id)
{
	return id->expansions->len;
}

gchar const *
cpg_expansion_get (CpgExpansion *id,
                   gint           idx)
{
	if (idx < 0 || idx >= id->expansions->len)
	{
		return NULL;
	}

	return (gchar const *)g_ptr_array_index (id->expansions, idx);
}

void
cpg_expansion_set (CpgExpansion *id,
                   gint           idx,
                   gchar const   *val)
{
	if (idx >= 0 && idx < id->expansions->len)
	{
		g_free (g_ptr_array_index (id->expansions, idx));
		id->expansions->pdata[idx] = g_strdup (val);
	}
}

void
cpg_expansion_free (CpgExpansion *id)
{
	gint i;

	if (!id)
	{
		return;
	}

	for (i = 0; i < id->expansions->len; ++i)
	{
		g_free (g_ptr_array_index (id->expansions, i));
	}

	g_slice_free (CpgExpansion, id);
}

CpgExpansion *
cpg_expansion_copy (CpgExpansion *id)
{
	CpgExpansion *ret;
	GPtrArray *ptr;
	gint i;

	if (id == NULL)
	{
		return NULL;
	}

	ptr = g_ptr_array_sized_new (cpg_expansion_num (id) + 1);

	for (i = 0; i < cpg_expansion_num (id); ++i)
	{
		g_ptr_array_add (ptr,
		                 g_strdup (cpg_expansion_get (id, i)));
	}

	ret = g_slice_new (CpgExpansion);
	ret->expansions = ptr;

	return ret;
}

void
cpg_expansion_add (CpgExpansion *id,
                   gchar const  *item)
{
	if (!id || !item)
	{
		return;
	}

	g_ptr_array_add (id->expansions, g_strdup (item));
}
