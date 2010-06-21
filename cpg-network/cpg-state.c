#include "cpg-state.h"
#include "cpg-expression.h"
#include "cpg-link.h"
#include "cpg-debug.h"

/**
 * SECTION:cpg-state
 * @short_description: Basic simulation state object
 *
 * The #CpgState is the basic simulation state object. A #CpgState updates
 * the target #CpgProperty from links that are connected to it at each timestep.
 *
 */

#define CPG_STATE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_STATE, CpgStatePrivate))

/*struct _CpgStatePrivate
{
};*/

G_DEFINE_TYPE (CpgState, cpg_state, CPG_TYPE_OBJECT)

static void
cpg_state_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_state_parent_class)->finalize (object);
}

static void
cpg_state_class_init (CpgStateClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_state_finalize;

	//g_type_class_add_private (object_class, sizeof (CpgStatePrivate));
}

static void
cpg_state_init (CpgState *self)
{
	//self->priv = CPG_STATE_GET_PRIVATE (self);
}

/**
 * cpg_state_new:
 * @id: the state id
 *
 * Creates a new #CpgState object
 *
 * Returns: the new #CpgState object
 **/
CpgState *
cpg_state_new (gchar const *id)
{
	return g_object_new (CPG_TYPE_STATE, "id", id, NULL);
}
