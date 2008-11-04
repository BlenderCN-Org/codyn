#include "cpg-thread.h"
#include "cpg-utils.h"
#include <pthread.h>
#include <stdlib.h>

struct _CpgThreadPrivate
{
	pthread_t id;
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
	//int ret = pthread_create(&(thread->priv->id), NULL, thread->function, data);
	thread->function(data);
	return 1;
	
	//thread->priv->running = (ret == 0);
	//return thread->priv->running;
}

int
cpg_thread_join(CpgThread *thread, void **retptr)
{
	/*if (!thread->priv->running)
		return 0;
	
	pthread_join(thread->priv->id, retptr);
	thread->priv->running = 0;*/
	
	return 1;
}
