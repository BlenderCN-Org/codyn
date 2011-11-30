/*
 * cdn-input-file.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_INPUT_FILE_H__
#define __CDN_INPUT_FILE_H__

#include <codyn/cdn-input.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define CDN_TYPE_INPUT_FILE		(cdn_input_file_get_type ())
#define CDN_INPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_FILE, CdnInputFile))
#define CDN_INPUT_FILE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_FILE, CdnInputFile const))
#define CDN_INPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INPUT_FILE, CdnInputFileClass))
#define CDN_IS_INPUT_FILE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INPUT_FILE))
#define CDN_IS_INPUT_FILE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INPUT_FILE))
#define CDN_INPUT_FILE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INPUT_FILE, CdnInputFileClass))

typedef struct _CdnInputFile		CdnInputFile;
typedef struct _CdnInputFileClass	CdnInputFileClass;
typedef struct _CdnInputFilePrivate	CdnInputFilePrivate;

struct _CdnInputFile
{
	/*< private >*/
	CdnInput parent;

	CdnInputFilePrivate *priv;
};

struct _CdnInputFileClass
{
	/*< private >*/
	CdnInputClass parent_class;
};

GType cdn_input_file_get_type (void) G_GNUC_CONST;

CdnInputFile *cdn_input_file_new (gchar const *id,
                                  GFile        *file);

CdnInputFile *cdn_input_file_new_for_path (gchar const *id,
                                           gchar const *filename);

GFile *cdn_input_file_get_file (CdnInputFile *input);
void cdn_input_file_set_file (CdnInputFile *input,
                              GFile        *file);

gchar *cdn_input_file_get_file_path (CdnInputFile *input);
void cdn_input_file_set_file_path (CdnInputFile *input,
                                   gchar const  *path);

void cdn_input_file_set_columns (CdnInputFile       *input,
                                 gchar const * const *names);

gchar **cdn_input_file_get_columns (CdnInputFile *input);

gboolean cdn_input_file_get_repeat (CdnInputFile *input);
void cdn_input_file_set_repeat (CdnInputFile *input,
                                gboolean      repeat);

gint cdn_input_file_get_time_column (CdnInputFile *input,
                                     gboolean     *isset);
void cdn_input_file_set_time_column (CdnInputFile *input,
                                     gint          column);

gdouble const * const *cdn_input_file_get_data (CdnInputFile *input,
                                                guint         *num_rows,
                                                guint         *num_columns);

gboolean cdn_input_file_ensure (CdnInputFile  *file,
                                GError       **error);

G_END_DECLS

#endif /* __CDN_INPUT_FILE_H__ */
