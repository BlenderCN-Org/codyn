#include "cpg-thread.h"
#include "cpg-utils.h"

#ifdef ENABLE_THREADS
#include <pthread.h>
#endif

#include <stdlib.h>

struct _CpgThreadPrivate
{
#ifdef ENABLE_THREADS
	pthread_t id;
#endif
	int running;
};

CpgThread *
cpg_thread_new(CpgThreadFunc function)
{
	CpgThread *ret = cpg_new1(CpgThread);
	ret->function = function;
	ret->priv = cpg_new1(CpgThreadPrivate);
	
	ret->priv->running = 0;
	
	return ret;
}

void
cpg_thread_free(CpgThread *thread)
{
	free(thread->priv);
	free(thread);
}

int
cpg_thread_run(CpgThread *thread, void *data)
{
#ifdef ENABLE_THREADS
	int ret = pthread_create(&(thread->priv->id), NULL, thread->function, data);
	thread->priv->running = (ret == 0);
	return thread->priv->running;
#else
	thread->function(data);
	return 1;
#endif
}

int
cpg_thread_join(CpgThread *thread, void **retptr)
{
#ifdef ENABLE_THREADS
	if (!thread->priv->running)
		return 0;
	
	pthread_join(thread->priv->id, retptr);
	thread->priv->running = 0;
#endif

	return 1;
}
