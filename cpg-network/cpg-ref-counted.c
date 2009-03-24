#include "cpg-ref-counted-private.h"

GType 
cpg_ref_counted_register_static(gchar const *name)
{
	return g_boxed_type_register_static(name, cpg_ref_counted_ref, cpg_ref_counted_unref);
}

void
cpg_ref_counted_init(gpointer       ref_counted,
					 GDestroyNotify destroy_func)
{
	CpgRefCounted *r = (CpgRefCounted *)ref_counted;

	r->ref_count = 1;
	r->destroy_func = destroy_func;
}

gpointer
cpg_ref_counted_ref(gpointer ref_counted)
{
	if (!ref_counted)
		return ref_counted;

	++(((CpgRefCounted *)ref_counted)->ref_count);
	return ref_counted;
}

void 
cpg_ref_counted_unref(gpointer ref_counted)
{
	CpgRefCounted *r = (CpgRefCounted *)ref_counted;
	
	if (!ref_counted || r->ref_count == 0)
		return;

	if (--r->ref_count == 0)
	{
		if (r->destroy_func)
			r->destroy_func(ref_counted);
	}
}

