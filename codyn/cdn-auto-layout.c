#include "cdn-auto-layout.h"
#include "cdn-layoutable.h"
#include "cdn-edge.h"
#include <string.h>
#include <math.h>

typedef enum
{
	CONSTRAINT_TYPE_RELATIVE_POSITION,
	CONSTRAINT_TYPE_EDGE,
	CONSTRAINT_TYPE_FIXED
} ConstraintType;

typedef struct
{
	CdnObject *object;

	gdouble x;
	gdouble y;

	gdouble px;
	gdouble py;

	gdouble cx;
	gdouble cy;

	GSList *edges;
} LayoutObject;

typedef struct
{
	LayoutObject *a;
	LayoutObject *b;
} ConstraintEdge;

typedef struct
{
	LayoutObject *object;

	gdouble x;
	gdouble y;
} ConstraintFixed;

typedef struct {
	LayoutObject *a;
	LayoutObject *b;

	gint dx;
	gint dy;
} ConstraintRelative;

typedef struct
{
	ConstraintType type;

	union
	{
		ConstraintEdge edge;
		ConstraintFixed fixed;
		ConstraintRelative relative;
	} c;
} Constraint;

static LayoutObject *
layout_object_new (CdnLayoutable *l)
{
	LayoutObject *ret;
	gint x;
	gint y;

	ret = g_slice_new0 (LayoutObject);

	ret->object = CDN_OBJECT (l);

	cdn_layoutable_get_location (l, &x, &y);

	ret->x = x;
	ret->px = x;

	ret->y = y;
	ret->py = y;

	return ret;
}

static void
layout_object_free (LayoutObject *o)
{
	g_slice_free (LayoutObject, o);
}

static void
extract_parts (CdnNode *node, GSList **layoutables, GSList **edges, GHashTable **names, GHashTable **node_map)
{
	GSList const *children;
	GSList *edge;

	*layoutables = NULL;

	children = cdn_node_get_children (node);

	*names = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	*node_map = g_hash_table_new (g_direct_hash, g_direct_equal);

	while (children)
	{
		CdnObject *c = children->data;

		if (CDN_IS_LAYOUTABLE (c))
		{
			CdnLayoutable *l = CDN_LAYOUTABLE (c);

			if (cdn_layoutable_supports_location (l))
			{
				LayoutObject *lo;

				lo = layout_object_new (l);
				g_hash_table_insert (*names, g_strdup (cdn_object_get_id (c)), lo);

				*layoutables = g_slist_prepend (*layoutables, lo);
				g_hash_table_insert (*node_map, l, lo);
			}
		}

		if (CDN_IS_EDGE (c))
		{
			*edges = g_slist_prepend (*edges, c);
		}

		children = g_slist_next (children);
	}

	*layoutables = g_slist_reverse (*layoutables);
	*edges = g_slist_reverse (*edges);

	for (edge = *edges; edge; edge = g_slist_next (edge))
	{
		CdnEdge *e = edge->data;
		LayoutObject *loi;
		LayoutObject *loo;

		loi = g_hash_table_lookup (*node_map, cdn_edge_get_input (e));
		loo = g_hash_table_lookup (*node_map, cdn_edge_get_output (e));

		if (!g_slist_find (loi->edges, loo))
		{
			loi->edges = g_slist_prepend (loi->edges, loo);
		}

		if (!g_slist_find (loo->edges, loi))
		{
			loo->edges = g_slist_prepend (loo->edges, loi);
		}
	}
}

static Constraint *
constraint_new (ConstraintType type)
{
	Constraint *c = g_slice_new0 (Constraint);
	c->type = type;

	return c;
}

static void
constraint_free (Constraint *c)
{
	g_slice_free (Constraint, c);
}

static void
extract_edge_constraints (GSList **ret, GSList const *edges, GHashTable *node_map)
{
	while (edges)
	{
		CdnEdge *e = edges->data;
		Constraint *c;

		c = constraint_new (CONSTRAINT_TYPE_EDGE);
		c->c.edge.a = g_hash_table_lookup (node_map, cdn_edge_get_input (e));
		c->c.edge.b = g_hash_table_lookup (node_map, cdn_edge_get_output (e));

		*ret = g_slist_prepend (*ret, c);

		edges = g_slist_next (edges);
	}
}

static void
extract_fixed_constraints (GSList **ret, GSList const *layoutables)
{
	while (layoutables)
	{
		LayoutObject *lo = layoutables->data;
		CdnLayoutable *l = CDN_LAYOUTABLE (lo->object);

		if (cdn_layoutable_get_has_location (l))
		{
			Constraint *c;
			gint x;
			gint y;

			c = constraint_new (CONSTRAINT_TYPE_FIXED);
			c->c.fixed.object = lo;

			cdn_layoutable_get_location (l, &x, &y);

			c->c.fixed.x = x;
			c->c.fixed.y = y;

			*ret = g_slist_prepend (*ret, c);
		}

		layoutables = g_slist_next (layoutables);
	}
}

typedef struct
{
	gchar const *first;
	gchar const *second;
	gint dx;
	gint dy;
} NamePair;

static NamePair name_pairs[] = {
	{"left", "right", 2, 0},
	{"down", "up", 0, -2},
	{"hind", "front", 0, -2},
	{"back", "front", 0, -2}
};

#define NUM_NAME_PAIRS (sizeof(name_pairs) / sizeof(NamePair))

static gboolean
name_pair_match (NamePair const *p, gchar const *name, gchar **symname)
{
	gint l = strlen (name);
	gint lf = strlen (p->first);
	gchar *lcase;
	gchar *found;
	gint i;
	gboolean ret = TRUE;

	lcase = g_ascii_strdown (name, -1);
	found = strstr (lcase, p->first);

	*symname = NULL;

	if (found == NULL)
	{
		g_free (lcase);
		return FALSE;
	}

	i = found - lcase;

	if (g_ascii_isupper (name[i]))
	{
		// Check for bound before
		if (found != lcase && !(g_ascii_isalpha (name[i - 1]) || g_ascii_islower (name[i - 1])))
		{
			ret = FALSE;
		}
		else if (i + lf != l && !(g_ascii_isalpha (name[i + lf]) || g_ascii_isupper(name[i + lf])))
		{
			ret = FALSE;
		}
		else
		{
			gchar *tmp = g_strndup (name, i);

			gchar uc[2] = {'\0', '\0'};
			uc[0] = g_ascii_toupper (p->second[0]);

			*symname = g_strconcat (tmp, uc, p->second + 1, name + i + lf, NULL);
			g_free (tmp);
		}
	}
	else
	{
		if (found != lcase && g_ascii_isalpha (name[i - 1]))
		{
			ret = FALSE;
		}
		else if (i + lf != l && g_ascii_isalpha (name[i + lf]))
		{
			ret = FALSE;
		}
		else
		{
			gchar *tmp = g_strndup (name, i);
			*symname = g_strconcat (tmp, p->second, name + i + lf, NULL);
			g_free (tmp);
		}
	}

	g_free (lcase);
	return ret;
}

static void
extract_relative_constraints (GSList **ret, GSList const *layoutables, GHashTable *names)
{
	while (layoutables)
	{
		LayoutObject *lo = layoutables->data;
		CdnObject *o = lo->object;
		gint i;

		for (i = 0; i < NUM_NAME_PAIRS; i++)
		{
			NamePair *p = &name_pairs[i];
			gchar *symname;

			if (name_pair_match (p, cdn_object_get_id (o), &symname))
			{
				LayoutObject *sym;

				sym = g_hash_table_lookup (names, symname);

				if (sym)
				{
					Constraint *c;

					c = constraint_new (CONSTRAINT_TYPE_RELATIVE_POSITION);
					c->c.relative.a = lo;
					c->c.relative.b = sym;
					c->c.relative.dx = p->dx;
					c->c.relative.dy = p->dy;

					*ret = g_slist_prepend (*ret, c);
				}

				g_free (symname);
			}
		}

		layoutables = g_slist_next (layoutables);
	}
}

static GSList *
extract_constraints (GSList const *layoutables, GSList const *edges, GHashTable *names, GHashTable *node_map)
{
	GSList *constraints = NULL;

	extract_fixed_constraints (&constraints, layoutables);
	extract_edge_constraints (&constraints, edges, node_map);
	extract_relative_constraints (&constraints, layoutables, names);

	return constraints;
}

static void
move_relative (LayoutObject *a, LayoutObject *b, gdouble dx, gdouble dy)
{
	CdnLayoutable *la = CDN_LAYOUTABLE (a->object);
	CdnLayoutable *lb = CDN_LAYOUTABLE (b->object);

	gboolean fa = cdn_layoutable_get_has_location (la);
	gboolean fb = cdn_layoutable_get_has_location (lb);

	if (fa && fb)
	{
		return;
	}

	if (fa)
	{
		// Move only b
		b->x += dx;
		b->y += dy;
	}
	else if (fb)
	{
		// Move only a
		a->x -= dx;
		a->y -= dy;
	}
	else
	{
		// Move both
		gint hx = dx / 2;
		gint hy = dy / 2;

		b->x += hx;
		b->y += hy;

		a->x -= (dx - hx);
		a->y -= (dy - hy);
	}
}

static void
apply_constraint_fixed (ConstraintFixed *fixed)
{
	if (fixed->object->x != fixed->x)
	{
		fixed->object->x = fixed->x;
	}

	if (fixed->object->y != fixed->y)
	{
		fixed->object->y = fixed->y;
	}
}

static gdouble
distance_between_objects (LayoutObject *a, LayoutObject *b)
{
	gdouble dx;
	gdouble dy;

	dx = b->x - a->x;
	dy = b->y - a->y;

	return sqrt (dx * dx + dy * dy);
}

static gdouble
angle_between_objects (LayoutObject *a, LayoutObject *b)
{
	gdouble dx;
	gdouble dy;

	dx = b->x - a->x;
	dy = b->y - a->y;

	return atan2 (dy, dx);
}

static void
apply_constraint_edge_linear (LayoutObject *a, LayoutObject *b, gdouble K, gdouble ldes, gboolean attract)
{
	gdouble dx = b->x - a->x;
	gdouble dy = b->y - a->y;
	gdouble l;

	if (dx == 0 && dy == 0)
	{
		gdouble nl;

		dx = ((gdouble)random() / (gdouble)RAND_MAX) * 2 - 1;
		dy = ((gdouble)random() / (gdouble)RAND_MAX) * 2 - 1;

		nl = sqrt (dx * dx + dy * dy);

		dx /= nl;
		dy /= nl;

		l = 0;
	}
	else
	{
		l = distance_between_objects (a, b);
	}

	if (!attract && l >= ldes)
	{
		return;
	}

	gdouble f;

	if (l != 0)
	{
		f = K * (ldes - l) / l;
	}
	else
	{
		f = K * ldes;
	}

	dx = f * dx;
	dy = f * dy;

	move_relative (a, b, dx, dy);
}

static void
move_rotate (LayoutObject *o, gdouble cx, gdouble cy, gdouble a)
{
	gdouble nx;
	gdouble ny;

	nx = cos (a) * (o->x - cx) - sin (a) * (o->y - cy) + cx;
	ny = sin (a) * (o->x - cx) + cos (a) * (o->y - cy) + cy;

	o->x = nx;
	o->y = ny;
}

static void
apply_constraint_edge_torsional (ConstraintEdge *edge, CdnAutoLayoutSettings settings)
{
	gdouble a;
	gdouble mp2;
	gdouble mp4;
	gdouble da;
	gdouble at;
	gboolean af;
	gboolean bf;
	gdouble cx;
	gdouble cy;

	af = cdn_layoutable_get_has_location (CDN_LAYOUTABLE (edge->a->object));
	bf = cdn_layoutable_get_has_location (CDN_LAYOUTABLE (edge->b->object));

	if (af && bf)
	{
		return;
	}

	a = angle_between_objects (edge->a, edge->b);

	if (fabs (a) < 1e-2)
	{
		return;
	}

	mp2 = M_PI / 2;
	mp4 = M_PI / 4;

	da = fmod (a, mp2);
	at = a - da;

	if (da > mp4 || da < -mp4)
	{
		at += copysign (mp2, da);
	}

	da = settings.edge_align_factor * (at - a);

	if (af)
	{
		cx = edge->a->x;
		cy = edge->a->y;
	}
	else if (bf)
	{
		cx = edge->b->x;
		cy = edge->b->y;
	}
	else
	{
		// Rotate both around center point
		cx = (edge->a->x + edge->b->x) / 2;
		cy = (edge->a->y + edge->b->y) / 2;
	}

	move_rotate (edge->a, cx, cy, da);
	move_rotate (edge->b, cx, cy, da);
}

static void
apply_constraint_edge (ConstraintEdge *edge, CdnAutoLayoutSettings settings)
{
	apply_constraint_edge_linear (edge->a, edge->b, settings.edge_distance_factor, settings.edge_rest_length, TRUE);
	apply_constraint_edge_torsional (edge, settings);
}

static void
apply_constraint_relative (ConstraintRelative *relative, CdnAutoLayoutSettings settings)
{
	LayoutObject *a = relative->a;
	LayoutObject *b = relative->b;
	gdouble K;

	gdouble dx = b->x - a->x;

	if (relative->dx != 0)
	{
		K = settings.relative_position_factor;
	}
	else
	{
		K = settings.relative_position_factor / 4;
	}

	if (dx == 0 || (dx > 0) != (relative->dx > 0))
	{
		// Constraint violated in x
		move_relative (a, b, K * (relative->dx - dx), 0);
	}

	gdouble dy = b->y - a->y;

	if (relative->dy != 0)
	{
		K = settings.relative_position_factor;
	}
	else
	{
		K = settings.relative_position_factor / 4;
	}

	if (dy == 0 || (dy > 0) != (relative->dy > 0))
	{
		// Constraint violated in y
		move_relative (a, b, 0, K * (relative->dy - dy));
	}
}

static void
apply_constraint_node_spread (GSList const *layoutables, CdnAutoLayoutSettings settings)
{
	while (layoutables)
	{
		GSList const *other;
		LayoutObject *a = layoutables->data;

		for (other = g_slist_next (layoutables); other; other = g_slist_next (other))
		{
			LayoutObject *b = other->data;

			apply_constraint_edge_linear (a, b, settings.node_spread_factor, settings.minimum_node_distance, FALSE);
		}

		layoutables = g_slist_next (layoutables);
	}
}

static void
apply_constraint_edge_spread (LayoutObject *lo, CdnAutoLayoutSettings settings)
{
	GSList *edge;
	gint n;
	gdouble ma;

	n = g_slist_length (lo->edges);

	if (n == 0)
	{
		return;
	}

	if (n <= 4)
	{
		ma = M_PI / 2;
	}
	else
	{
		ma = (2 * M_PI) / n;
	}

	for (edge = lo->edges; edge; edge = g_slist_next (edge))
	{
		LayoutObject *a = edge->data;
		GSList *other;

		for (other = g_slist_next (edge); other; other = g_slist_next (other))
		{
			LayoutObject *b = other->data;
			gdouble da;
			gdouble ab;
			gdouble aa;

 			aa = angle_between_objects (lo, a);
			ab = angle_between_objects (lo, b);

			da = fmod (ab - aa, M_PI);

			if (fabs (da) < ma)
			{
				// Push apart
				gdouble ta;

				if (da > 0)
				{
					ta = ma - da;
				}
				else
				{
					ta = -ma - da;
				}

				ta = settings.edge_spread_factor * ta;

				move_rotate (a, lo->x, lo->y, -ta / 2);
				move_rotate (b, lo->x, lo->y, ta / 2);
			}
		}
	}
}

static void
save_current_constraint_pos (GSList const *layoutables)
{
	while (layoutables)
	{
		LayoutObject *lo = layoutables->data;

		lo->cx = lo->x;
		lo->cy = lo->y;

		layoutables = g_slist_next (layoutables);
	}
}

static gboolean
constraints_were_active (GSList const *layoutables)
{
	gboolean ret = FALSE;

	while (layoutables)
	{
		LayoutObject *lo = layoutables->data;

		if (fabs (lo->x - lo->cx) > 1e-2 || fabs (lo->y - lo->cy) > 1e-2)
		{
			ret = TRUE;
			break;
		}

		layoutables = g_slist_next (layoutables);
	}

	return ret;
}

static gboolean
apply_constraints (GSList const *layoutables, GSList const *constraints, CdnAutoLayoutSettings settings)
{
	GSList const *l;

	save_current_constraint_pos (layoutables);

	while (constraints)
	{
		Constraint *c = constraints->data;

		switch (c->type)
		{
		case CONSTRAINT_TYPE_FIXED:
			apply_constraint_fixed (&c->c.fixed);
			break;
		case CONSTRAINT_TYPE_EDGE:
			apply_constraint_edge (&c->c.edge, settings);
			break;
		case CONSTRAINT_TYPE_RELATIVE_POSITION:
			apply_constraint_relative (&c->c.relative, settings);
			break;
		}

		constraints = g_slist_next (constraints);
	}

	for (l = layoutables; l; l = g_slist_next (l))
	{
		LayoutObject *lo = l->data;

		apply_constraint_edge_spread (lo, settings);
	}

	apply_constraint_node_spread (layoutables, settings);

	// Attract towards center
	for (l = layoutables; l; l = g_slist_next (l))
	{
		LayoutObject *lo = l->data;

		lo->x -= lo->x * settings.centering_factor;
		lo->y -= lo->y * settings.centering_factor;
	}

	return constraints_were_active (layoutables);
}

static gboolean
apply_inertia (GSList const *layoutables, CdnAutoLayoutSettings settings)
{
	gboolean ret = FALSE;

	while (layoutables)
	{
		LayoutObject *o = layoutables->data;
		gdouble px;
		gdouble py;
		gdouble dx;
		gdouble dy;

		px = o->x;
		py = o->y;

		dx = o->x - o->px;
		dy = o->y - o->py;

		if (fabs(dx) + fabs(dy) > 1e-2)
		{
			ret = TRUE;
		}

		gdouble D = settings.inertia_factor;

		o->x += D * dx;
		o->y += D * dy;

		o->px = px;
		o->py = py;

		layoutables = g_slist_next (layoutables);
	}

	return ret;
}

static void
apply_centering (GSList *layoutables)
{
	GSList *item;
	gdouble x = 0;
	gdouble y = 0;
	gint n = 0;

	for (item = layoutables; item; item = g_slist_next (item))
	{
		LayoutObject *lo = item->data;

		x += lo->x;
		y += lo->y;

		n++;
	}

	if (n == 0)
	{
		return;
	}

	x /= n;
	y /= n;

	for (item = layoutables; item; item = g_slist_next (item))
	{
		LayoutObject *lo = item->data;

		lo->x -= x;
		lo->y -= y;

		lo->px -= x;
		lo->py -= y;

		lo->cx -= x;
		lo->cy -= y;
	}
}

static void
init_layout (GSList *layoutables, GSList *constraints, CdnAutoLayoutSettings settings)
{
	gint i;
	GSList *item;
	guint h = 0;
	gdouble r;

	i = 0;

	for (item = layoutables; item; item = g_slist_next (item))
	{
		LayoutObject *lo = item->data;
		gchar const *id = cdn_object_get_id (lo->object);

		h ^= g_str_hash (id);
		i++;
	}

	srandom (h);
	r = ceil (sqrt(i));

	for (item = layoutables; item; item = g_slist_next (item))
	{
		LayoutObject *lo = item->data;

		if (cdn_layoutable_get_has_location (CDN_LAYOUTABLE (lo->object)))
		{
			continue;
		}

		lo->x = ((gdouble)random () / (gdouble)RAND_MAX) * (r * 2) - r;
		lo->y = ((gdouble)random () / (gdouble)RAND_MAX) * (r * 2) - r;
	}

	for (i = 0; i < 30; i++)
	{
		save_current_constraint_pos (layoutables);
		apply_constraint_node_spread (layoutables, settings);

		for (item = constraints; item; item = g_slist_next (item))
		{
			Constraint *c = item->data;

			if (c->type == CONSTRAINT_TYPE_RELATIVE_POSITION)
			{
				apply_constraint_relative (&c->c.relative, settings);
			}
		}

		if (!constraints_were_active (layoutables))
		{
			break;
		}
	}

	apply_centering (layoutables);

	// Prevent inertia from intialization
	for (item = layoutables; item; item = g_slist_next (item))
	{
		LayoutObject *lo = item->data;

		lo->px = lo->x;
		lo->py = lo->y;
	}
}

void
cdn_auto_layout (CdnNode *node, CdnAutoLayoutSettings settings)
{
	GSList *layoutables = NULL;
	GSList *edges = NULL;
	GHashTable *names;
	GHashTable *node_map;
	GSList *constraints;
	gint i;
	gint k;

	extract_parts (node, &layoutables, &edges, &names, &node_map);
	constraints = extract_constraints (layoutables, edges, names, node_map);

	init_layout (layoutables, constraints, settings);

	for (k = 0; k < 100; k++)
	{
		for (i = 0; i < 30; i++)
		{
			if (!apply_constraints (layoutables, constraints, settings))
			{
				break;
			}
		}

		apply_centering (layoutables);

		if (!apply_inertia (layoutables, settings))
		{
			break;
		}
	}

	while (layoutables)
	{
		LayoutObject *lo = layoutables->data;

		cdn_layoutable_set_location (CDN_LAYOUTABLE(lo->object), lrint (lo->x), lrint (lo->y));

		layout_object_free (lo);
		layoutables = g_slist_delete_link (layoutables, layoutables);
	}

	g_slist_foreach (constraints, (GFunc)constraint_free, NULL);
	g_slist_free (constraints);

	g_slist_free (edges);

	g_hash_table_destroy (names);
	g_hash_table_destroy (node_map);
}

CdnAutoLayoutSettings
cdn_auto_layout_settings_default ()
{
	CdnAutoLayoutSettings ret;

	ret.edge_align_factor = 0.01;
	ret.edge_distance_factor = 0.2;
	ret.edge_spread_factor = 0.005;

	ret.node_spread_factor = 0.2;

	ret.relative_position_factor = 0.5;

	ret.inertia_factor = 0.01;
	ret.centering_factor = 0.01;

	ret.minimum_node_distance = 2;
	ret.edge_rest_length = 2.2;

	return ret;
}
