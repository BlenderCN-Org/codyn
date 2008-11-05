#include "cpg-mutex.h"
#include "cpg-utils.h"

#ifdef ENABLE_THREADS
#include <pthread.h>
#endif

struct _CpgMutexPriv
{
#ifdef ENABLE_THREADS
	pthread_mutex_t pmutex;
	pthread_cond_t pcond;
#else
	int notempty;
#endif
};

#ifdef ENABLE_THREADS
static void
initialize_pmutex(pthread_mutex_t *pmutex, int recursive)
{
	pthread_mutexattr_t mta;
	
	pthread_mutexattr_init(&mta);
	pthread_mutexattr_settype(&mta, recursive ? PTHREAD_MUTEX_RECURSIVE : PTHREAD_MUTEX_NORMAL);
	
	pthread_mutex_init(pmutex, &mta);
	pthread_mutexattr_destroy(&mta);
}
#endif

static CpgMutex *
create_mutex(int recursive)
{
	CpgMutex *res = cpg_new1(CpgMutex);
	res->priv = cpg_new1(CpgMutexPriv);
	
#ifdef ENABLE_THREADS
	initialize_pmutex(&(res->priv->pmutex), recursive);
	
	pthread_cond_init(&(res->priv->pcond), NULL);
#endif

	return res;
}

CpgMutex *
cpg_mutex_new()
{
	return create_mutex(0);
}

CpgMutex *
cpg_mutex_new_recursive()
{
	return create_mutex(1);
}

int
cpg_mutex_lock(CpgMutex *mutex)
{
#ifdef ENABLE_THREADS
	if (mutex)
		return pthread_mutex_lock(&(mutex->priv->pmutex)) == 0;
#endif
	return 1;
}

int
cpg_mutex_unlock(CpgMutex *mutex)
{
#ifdef ENABLE_THREADS
	if (mutex)
		return pthread_mutex_unlock(&(mutex->priv->pmutex)) == 0;
#endif
	return 1;
}

int
cpg_mutex_try_lock(CpgMutex *mutex)
{
#ifdef ENABLE_THREADS
	if (mutex)
		return pthread_mutex_trylock(&(mutex->priv->pmutex)) == 0;
#endif
	return 1;
}

void
cpg_mutex_cond_wait(CpgMutex *mutex)
{
#ifdef ENABLE_THREADS
	if (mutex)
		pthread_cond_wait(&(mutex->priv->pcond), &(mutex->priv->pmutex));
#endif
}

void
cpg_mutex_cond_signal(CpgMutex *mutex)
{
#ifdef ENABLE_THREADS
	if (mutex)
		pthread_cond_signal(&(mutex->priv->pcond));
#endif
}

void
cpg_mutex_free(CpgMutex *mutex)
{
	if (!mutex)
		return;

#ifdef ENABLE_THREADS
	pthread_mutex_destroy(&(mutex->priv->pmutex));
	pthread_cond_destroy(&(mutex->priv->pcond));
#endif

	free(mutex->priv);
	free(mutex);
}
