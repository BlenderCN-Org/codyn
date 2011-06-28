#ifndef __CPG_INPUT_FILE_H__
#define __CPG_INPUT_FILE_H__

#include <cpg-network/cpg-input.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define CPG_TYPE_INPUT_FILE		(cpg_input_file_get_type ())
#define CPG_INPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INPUT_FILE, CpgInputFile))
#define CPG_INPUT_FILE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INPUT_FILE, CpgInputFile const))
#define CPG_INPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INPUT_FILE, CpgInputFileClass))
#define CPG_IS_INPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INPUT_FILE))
#define CPG_IS_INPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INPUT_FILE))
#define CPG_INPUT_FILE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INPUT_FILE, CpgInputFileClass))

typedef struct _CpgInputFile		CpgInputFile;
typedef struct _CpgInputFileClass	CpgInputFileClass;
typedef struct _CpgInputFilePrivate	CpgInputFilePrivate;

struct _CpgInputFile
{
	/*< private >*/
	CpgInput parent;

	CpgInputFilePrivate *priv;
};

struct _CpgInputFileClass
{
	/*< private >*/
	CpgInputClass parent_class;
};

GType cpg_input_file_get_type (void) G_GNUC_CONST;

CpgInputFile *cpg_input_file_new (gchar const *id,
                                  GFile        *file);

CpgInputFile *cpg_input_file_new_for_path (gchar const *id,
                                           gchar const *filename);

GFile *cpg_input_file_get_file (CpgInputFile *input);
void cpg_input_file_set_file (CpgInputFile *input,
                              GFile        *file);

gchar *cpg_input_file_get_file_path (CpgInputFile *input);
void cpg_input_file_set_file_path (CpgInputFile *input,
                                   gchar const  *path);

void cpg_input_file_set_columns (CpgInputFile       *input,
                                 gchar const * const *names);

gchar **cpg_input_file_get_columns (CpgInputFile *input);

gboolean cpg_input_file_get_repeat (CpgInputFile *input);
void cpg_input_file_set_repeat (CpgInputFile *input,
                                gboolean      repeat);

gint cpg_input_file_get_time_column (CpgInputFile *input,
                                     gboolean     *isset);
void cpg_input_file_set_time_column (CpgInputFile *input,
                                     gint          column);

gdouble const * const *cpg_input_file_get_data (CpgInputFile *input,
                                                guint         *num_rows,
                                                guint         *num_columns);

gboolean cpg_input_file_ensure (CpgInputFile  *file,
                                GError       **error);

G_END_DECLS

#endif /* __CPG_INPUT_FILE_H__ */
