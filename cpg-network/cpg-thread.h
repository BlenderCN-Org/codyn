#ifndef __CPG_THREAD_H__
#define __CPG_THREAD_H__

typedef struct _CpgThreadPrivate CpgThreadPrivate;

typedef void *(*CpgThreadFunc)(void *);

typedef struct
{
	CpgThreadFunc function;
	CpgThreadPrivate *priv;
} CpgThread;

CpgThread *cpg_thread_new(CpgThreadFunc function);
void cpg_thread_free(CpgThread *thread);

int cpg_thread_run(CpgThread *thread, void *data);
int cpg_thread_join(CpgThread *thread, void **retval);

#endif /* __CPG_THREAD_H__ */

