#include "cpg-expansion.h"


#define CPG_EXPANSION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EXPANSION, CpgExpansionPrivate))

struct _CpgExpansionPrivate
{
	GPtrArray *expansions;
};

G_DEFINE_TYPE (CpgExpansion, cpg_expansion, G_TYPE_OBJECT)

static void
cpg_expansion_finalize (GObject *object)
{
	CpgExpansion *expansion;
	gint i;

	expansion = CPG_EXPANSION (object);

	for (i = 0; i < expansion->priv->expansions->len; ++i)
	{
		g_free (g_ptr_array_index (expansion->priv->expansions, i));
	}

	G_OBJECT_CLASS (cpg_expansion_parent_class)->finalize (object);
}

static void
cpg_expansion_class_init (CpgExpansionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_expansion_finalize;

	g_type_class_add_private (object_class, sizeof (CpgExpansionPrivate));
}

static void
cpg_expansion_init (CpgExpansion *self)
{
	self->priv = CPG_EXPANSION_GET_PRIVATE (self);
}

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

	ret = g_object_new (CPG_TYPE_EXPANSION, NULL);

	ret->priv->expansions = g_ptr_array_new ();

	while (items && *items)
	{
		g_ptr_array_add (ret->priv->expansions, g_strdup (*items));
		++items;
	}

	if (ret->priv->expansions->len == 0)
	{
		g_ptr_array_add (ret->priv->expansions, g_strdup (""));
	}

	return ret;
}

gint
cpg_expansion_num (CpgExpansion *id)
{
	g_return_val_if_fail (CPG_IS_EXPANSION (id), 0);

	return id->priv->expansions->len;
}

gchar const *
cpg_expansion_get (CpgExpansion *id,
                   gint           idx)
{
	g_return_val_if_fail (CPG_IS_EXPANSION (id), NULL);

	if (idx < 0 || idx >= id->priv->expansions->len)
	{
		return NULL;
	}

	return (gchar const *)g_ptr_array_index (id->priv->expansions, idx);
}

void
cpg_expansion_set (CpgExpansion *id,
                   gint           idx,
                   gchar const   *val)
{
	g_return_if_fail (CPG_IS_EXPANSION (id));

	if (idx >= 0 && idx < id->priv->expansions->len)
	{
		g_free (g_ptr_array_index (id->priv->expansions, idx));
		id->priv->expansions->pdata[idx] = g_strdup (val);
	}
}

CpgExpansion *
cpg_expansion_copy (CpgExpansion *id)
{
	CpgExpansion *ret;
	GPtrArray *ptr;
	gchar **args;
	gint i;

	g_return_val_if_fail (id == NULL || CPG_IS_EXPANSION (id), NULL);

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

	g_ptr_array_add (ptr, NULL);
	args = (gchar **)g_ptr_array_free (ptr, FALSE);

	ret = cpg_expansion_new ((gchar const * const *)args);
	g_strfreev (args);

	return ret;
}

void
cpg_expansion_add (CpgExpansion *id,
                   gchar const  *item)
{
	g_return_if_fail (CPG_IS_EXPANSION (id));
	g_return_if_fail (item != NULL);

	g_ptr_array_add (id->priv->expansions, g_strdup (item));
}
