#include <stdlib.h>
#include <unistd.h>
#include <string.h>

static void
prepend_env_path (char const *key, char const *value)
{
	char *cur;
	char *ret;
	int clen;
	int vlen;

	cur = getenv (key);

	if (cur == NULL)
	{
		setenv (key, value, 1);
		return;
	}

	clen = strlen (cur);
	vlen = strlen (value);

	ret = (char *)malloc((strlen (cur) + 1 + strlen (value) + 1) * sizeof (char *));

	strcpy (ret, value);
	ret[vlen] = ':';

	strcpy (ret + vlen + 1, cur);
	ret[vlen + clen + 1] = '\0';

	setenv (key, ret, 1);
	free (ret);
}

int
main (int argc, char const *argv[])
{
	char const **nargv;
	int i;

	nargv = (char const **)malloc((argc + 2) * sizeof (char const *));

	nargv[0] = "/usr/bin/env";
	nargv[1] = PYTHON;

	for (i = 1; i < argc; i++)
	{
		nargv[i + 1] = argv[i];
	}

	nargv[argc + 1] = NULL;

	prepend_env_path ("GI_TYPELIB_PATH", LIBDIR "/girepository-1.0");
	prepend_env_path ("GIR_DIRS", DATADIR "/gir-1.0");
	prepend_env_path ("PYTHONPATH", LIBDIR "/python2.7/site-packages");

#ifdef ENABLE_OSX_FRAMEWORK
	prepend_env_path ("DYLD_LIBRARY_PATH", PREFIX "/Libraries");
#endif

	execv (nargv[0], (char * const *)nargv);
	return 0;
}
