#include "cpg-ref-counted-private.h"

/**
 * cpg_ref_counted_init:
 * @ref_counted: a #CpgRefCounted
 * @destroy_func: callback to destroy the ref counted object
 *
 * Use this after a new ref counted object has been constructed. This
 * sets the callback to be used when the ref count decreases to 0. The function
 * sets the initial ref count to 1
 *
 **/
void
cpg_ref_counted_init (gpointer       ref_counted,
                      GDestroyNotify destroy_func)
{
	CpgRefCounted *r = (CpgRefCounted *)ref_counted;

	r->ref_count = 1;
	r->destroy_func = destroy_func;
}

/**
 * cpg_ref_counted_ref:
 * @ref_counted: a #CpgRefCounted
 *
 * Increase the ref count on @ref_counted.
 *
 * Returns: @ref_counted
 *
 **/
gpointer
cpg_ref_counted_ref (gpointer ref_counted)
{
	if (!ref_counted)
		return ref_counted;

	++(((CpgRefCounted *)ref_counted)->ref_count);
	return ref_counted;
}

/**
 * cpg_ref_counted_unref:
 * @ref_counted: a #CpgRefCounted
 *
 * Decrease the ref count on @ref_counted.
 *
 **/
void 
cpg_ref_counted_unref (gpointer ref_counted)
{
	CpgRefCounted *r = (CpgRefCounted *)ref_counted;
	
	if (!ref_counted || r->ref_count == 0)
		return;

	if (--r->ref_count == 0)
	{
		if (r->destroy_func)
			r->destroy_func (ref_counted);
	}
}

