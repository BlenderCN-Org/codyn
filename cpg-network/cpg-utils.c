#include "cpg-utils.h"

gboolean
cpg_signal_accumulator_false_handled (GSignalInvocationHint *ihint,
                                      GValue                *return_accu,
                                      const GValue          *handler_return,
                                      gpointer               dummy)
{
	gboolean continue_emission;
	gboolean signal_handled;

	signal_handled = g_value_get_boolean (handler_return);

	g_value_set_boolean (return_accu, signal_handled);
	continue_emission = signal_handled;

	return continue_emission;
}
