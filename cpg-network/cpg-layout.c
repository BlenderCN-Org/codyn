#include "cpg-layout.h"


#define CPG_LAYOUT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_LAYOUT, CpgLayoutPrivate))

typedef struct
{
	CpgObject *left;
	CpgObject *right;
	CpgLayoutRelation relation;
} Relation;

static Relation *
relation_new (CpgObject *left,
              CpgObject *right,
              CpgLayoutRelation relation)
{
	Relation *ret;

	ret = g_slice_new0 (Relation);

	ret->left = g_object_ref (left);
	ret->right = g_object_ref (right);
	ret->relation = relation;

	return ret;
}

static void
relation_free (Relation *self)
{
	g_object_unref (self->left);
	g_object_unref (self->right);

	g_slice_free (Relation, self);
}

struct _CpgLayoutPrivate
{
	CpgNetwork *network;

	GSList *relations;
};

G_DEFINE_TYPE (CpgLayout, cpg_layout, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK
};

static void
cpg_layout_finalize (GObject *object)
{
	CpgLayout *layout;

	layout = CPG_LAYOUT (object);

	g_slist_foreach (layout->priv->relations, (GFunc)relation_free, NULL);
	g_slist_free (layout->priv->relations);

	G_OBJECT_CLASS (cpg_layout_parent_class)->finalize (object);
}

static void
cpg_layout_dispose (GObject *object)
{
	CpgLayout *layout;

	layout = CPG_LAYOUT (object);

	if (layout->priv->network)
	{
		g_object_unref (layout->priv->network);
		layout->priv->network = NULL;
	}

	G_OBJECT_CLASS (cpg_layout_parent_class)->dispose (object);
}

static void
cpg_layout_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgLayout *self = CPG_LAYOUT (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			self->priv->network = g_value_dup_object (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_layout_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgLayout *self = CPG_LAYOUT (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			g_value_set_object (value, self->priv->network);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_layout_class_init (CpgLayoutClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_layout_finalize;
	object_class->dispose = cpg_layout_dispose;

	object_class->get_property = cpg_layout_get_property;
	object_class->set_property = cpg_layout_set_property;

	g_type_class_add_private (object_class, sizeof(CpgLayoutPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_layout_init (CpgLayout *self)
{
	self->priv = CPG_LAYOUT_GET_PRIVATE (self);
}

CpgLayout *
cpg_layout_new (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return g_object_new (CPG_TYPE_LAYOUT,
	                     "network", network,
	                     NULL);
}

void
cpg_layout_add (CpgLayout *layout,
                CpgObject *left,
                CpgObject *right,
                CpgLayoutRelation relation)
{
	g_return_if_fail (CPG_IS_LAYOUT (layout));
	g_return_if_fail (CPG_IS_OBJECT (left));
	g_return_if_fail (!CPG_IS_LINK (left));
	g_return_if_fail (CPG_IS_OBJECT (right));
	g_return_if_fail (!CPG_IS_LINK (right));

	layout->priv->relations =
		g_slist_prepend (layout->priv->relations,
		                 relation_new (left, right, relation));
}
