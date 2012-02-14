/*
 * cdn-network.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_NETWORK_H__
#define __CDN_NETWORK_H__

#include <gio/gio.h>

#include <codyn/cdn-object.h>
#include <codyn/cdn-edge.h>
#include <codyn/cdn-monitor.h>
#include <codyn/cdn-compile-error.h>
#include <codyn/cdn-function.h>
#include <codyn/integrators/cdn-integrator.h>
#include <codyn/cdn-node.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_NETWORK            (cdn_network_get_type ())
#define CDN_NETWORK(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK, CdnNetwork))
#define CDN_NETWORK_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK, CdnNetwork const))
#define CDN_NETWORK_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_NETWORK, CdnNetworkClass))
#define CDN_IS_NETWORK(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_NETWORK))
#define CDN_IS_NETWORK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_NETWORK))
#define CDN_NETWORK_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_NETWORK, CdnNetworkClass))

typedef struct _CdnNetwork        CdnNetwork;
typedef struct _CdnNetworkClass   CdnNetworkClass;
typedef struct _CdnNetworkPrivate CdnNetworkPrivate;

#define CDN_NETWORK_LOAD_ERROR (cdn_network_load_error_quark ())

/**
 * CdnNetworkLoadError:
 * @CDN_NETWORK_LOAD_ERROR_NONE:
 * @CDN_NETWORK_LOAD_ERROR_SYNTAX: syntax error
 * @CDN_NETWORK_LOAD_ERROR_VARIABLE: error occurred in loading a variable
 * @CDN_NETWORK_LOAD_ERROR_OBJECT: error occurred in loading an object
 * @CDN_NETWORK_LOAD_ERROR_EDGE: error occurred in loading a edge
 * @CDN_NETWORK_LOAD_ERROR_FUNCTION: error occurred in loading a function
 * @CDN_NETWORK_LOAD_ERROR_IMPORT: error occurred in an import
 * @CDN_NETWORK_LOAD_ERROR_INTERFACE: error occurred in an interface
 *
 * Network load error types.
 *
 */
typedef enum
{
	CDN_NETWORK_LOAD_ERROR_NONE,
	CDN_NETWORK_LOAD_ERROR_SYNTAX,
	CDN_NETWORK_LOAD_ERROR_VARIABLE,
	CDN_NETWORK_LOAD_ERROR_OBJECT,
	CDN_NETWORK_LOAD_ERROR_EDGE,
	CDN_NETWORK_LOAD_ERROR_FUNCTION,
	CDN_NETWORK_LOAD_ERROR_IMPORT,
	CDN_NETWORK_LOAD_ERROR_INPUT,
	CDN_NETWORK_LOAD_ERROR_INTERFACE,
	CDN_NETWORK_LOAD_ERROR_OPERATOR
} CdnNetworkLoadError;

typedef enum
{
	CDN_NETWORK_FORMAT_UNKNOWN,
	CDN_NETWORK_FORMAT_CDN,
	CDN_NETWORK_FORMAT_XML
} CdnNetworkFormat;

#define CDN_NETWORK_ERROR (cdn_network_error_quark ())

typedef enum
{
	CDN_NETWORK_ERROR_UNOWNED_TEMPLATE,
	CDN_NETWORK_ERROR_NUM
} CdnNetworkError;

struct _CdnNetwork
{
	/*< private >*/
	CdnNode parent;

	CdnNetworkPrivate *priv;
};

/**
 * CdnNetworkClass:
 * @compile_error: compile error default signal handler
 *
 * The CdnNetwork class
 *
 */
struct _CdnNetworkClass
{
	/*< private >*/
	CdnNodeClass parent_class;

	/*< public >*/
	void (*compile_error) (CdnNetwork      *network,
	                       CdnCompileError *error);
};

GType             cdn_network_get_type              (void) G_GNUC_CONST;

void              cdn_init                          (void);

GQuark            cdn_network_load_error_quark      (void);
GQuark            cdn_network_error_quark           (void);

CdnNetwork       *cdn_network_new                    (void);

CdnNetwork       *cdn_network_new_from_file          (GFile          *file,
                                                      GError        **error);

CdnNetwork       *cdn_network_new_from_stream        (GInputStream   *stream,
                                                      GError        **error);

CdnNetwork       *cdn_network_new_from_path          (const gchar    *path,
                                                      GError        **error);

CdnNetwork       *cdn_network_new_from_string        (const gchar    *s,
                                                      GError        **error);

gboolean          cdn_network_load_from_file         (CdnNetwork     *network,
                                                      GFile          *file,
                                                      GError        **error);

gboolean          cdn_network_load_from_stream       (CdnNetwork     *network,
                                                      GInputStream   *stream,
                                                      GError        **error);

gboolean          cdn_network_load_from_path         (CdnNetwork     *network,
                                                      const gchar    *path,
                                                      GError        **error);

gboolean          cdn_network_load_from_string       (CdnNetwork     *network,
                                                      const gchar    *s,
                                                      GError        **error);

CdnNetworkFormat  cdn_network_format_from_file       (GFile          *file);

CdnNetworkFormat  cdn_network_format_from_stream     (GInputStream   *stream);

GFile            *cdn_network_get_file               (CdnNetwork     *network);
gchar            *cdn_network_get_path               (CdnNetwork     *network);

void              cdn_network_set_integrator         (CdnNetwork     *network,
                                                      CdnIntegrator  *integrator);

CdnIntegrator    *cdn_network_get_integrator         (CdnNetwork     *network);

void              cdn_network_merge                  (CdnNetwork     *network,
                                                      CdnNetwork     *other);

void              cdn_network_merge_from_file        (CdnNetwork     *network,
                                                      GFile          *file,
                                                      GError        **error);

void              cdn_network_merge_from_path        (CdnNetwork     *network,
                                                      const gchar    *path,
                                                      GError        **error);

void              cdn_network_merge_from_string      (CdnNetwork     *network,
                                                      const gchar    *s,
                                                      GError        **error);

/* simulation functions */
gboolean          cdn_network_run                    (CdnNetwork  *network,
                                                      gdouble      from,
                                                      gdouble      timestep,
                                                      gdouble      to,
                                                      GError     **error);

gboolean          cdn_network_begin                  (CdnNetwork  *network,
                                                      gdouble      start,
                                                      GError     **error);

gboolean          cdn_network_end                    (CdnNetwork  *network,
                                                      GError     **error);

gdouble           cdn_network_step                   (CdnNetwork *network,
                                                      gdouble     timestep);

CdnNode         *cdn_network_get_template_node     (CdnNetwork   *network);

CdnImportForward *cdn_network_get_import  (CdnNetwork   *network,
                                                       GFile        *file);

CdnImportForward *cdn_network_get_import_from_path  (CdnNetwork   *network,
                                                                 const gchar  *path);


void              _cdn_network_register_import       (CdnNetwork        *network,
                                                      CdnImportForward *import);

G_END_DECLS

#endif /* __CDN_NETWORK_H__ */
