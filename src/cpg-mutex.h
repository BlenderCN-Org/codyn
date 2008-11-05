#ifndef __CPG_MUTEX_H__
#define __CPG_MUTEX_H__

typedef struct _CpgMutexPriv CpgMutexPriv;

typedef struct
{
	CpgMutexPriv *priv;
} CpgMutex;

CpgMutex *cpg_mutex_new();
CpgMutex *cpg_mutex_new_recursive();

void cpg_mutex_free();

int cpg_mutex_lock(CpgMutex *mutex);
int cpg_mutex_unlock(CpgMutex *mutex);
int cpg_mutex_try_lock(CpgMutex *mutex);

void cpg_mutex_cond_wait(CpgMutex *mutex);
void cpg_mutex_cond_signal(CpgMutex *mutex);

#endif /* __CPG_MUTEX_H__ */
