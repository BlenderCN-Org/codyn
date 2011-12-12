/*
 * cdn-object.h
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

#ifndef __CDN_OBJECT_H__
#define __CDN_OBJECT_H__

#include <glib-object.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-compile-context.h>
#include <codyn/cdn-utils.h>
#include <codyn/cdn-usable.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_OBJECT_ERROR (cdn_object_error_quark ())

#define CDN_TYPE_OBJECT            (cdn_object_get_type ())
#define CDN_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OBJECT, CdnObject))
#define CDN_OBJECT_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OBJECT, CdnObject const))
#define CDN_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_OBJECT, CdnObjectClass))
#define CDN_IS_OBJECT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_OBJECT))
#define CDN_IS_OBJECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_OBJECT))
#define CDN_OBJECT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_OBJECT, CdnObjectClass))

typedef struct _CdnObject			CdnObject;
typedef struct _CdnObjectClass		CdnObjectClass;
typedef struct _CdnObjectPrivate	CdnObjectPrivate;


/**
 * CdnObjectError:
 * @CDN_OBJECT_ERROR_VARIABLE_UNKNOWN: unknown
 * @CDN_OBJECT_ERROR_VARIABLE_NOT_FOUND: variable not found
 * @CDN_OBJECT_ERROR_VARIABLE_IN_USE: variable in use
 * @CDN_OBJECT_NUM_ERRORS: num errors
 *
 * Enum used to indicate an error when removing a variable
 *
 **/
typedef enum
{
	CDN_OBJECT_ERROR_VARIABLE_UNKNOWN,
	CDN_OBJECT_ERROR_VARIABLE_NOT_FOUND,
	CDN_OBJECT_ERROR_VARIABLE_IN_USE,
	CDN_OBJECT_ERROR_VARIABLE_FROM_TEMPLATE,
	CDN_OBJECT_ERROR_INVALID_VARIABLE_NAME,
	CDN_OBJECT_ERROR_TEMPLATE_ALREADY_APPLIED,
	CDN_OBJECT_ERROR_TEMPLATE_NOT_FOUND,
	CDN_OBJECT_NUM_ERRORS
} CdnObjectError;

struct _CdnObject
{
	/*< private >*/
	GObject parent;

	CdnObjectPrivate *priv;
};

typedef void (*CdnForeachExpressionFunc) (CdnExpression *expression, gpointer userdata);

/**
 * CdnObjectClass:
 * @compile: compile virtual function
 * @reset: reset virtual function
 * @foreach_expression: call callback for each expression
 * @reset_cache: reset cache virtual function
 * @apply_template: apply template virtual function
 * @unapply_template: unapply template virtual function
 * @copy: copy virtual function
 * @get_copy_type: get copy type virtual function
 * @taint: taint virtual function
 * @get_variables: get variables virtual function
 * @get_variable: get variable virtual function
 * @has_variable: has variable virtual function
 * @add_variable: add variable virtual function
 * @remove_variable: remove variable virtual function
 * @verify_remove_variable: verify remove variable virtual function
 * @clear: clear virtual function
 * @equal: equal virtual function
 * @compiled: compiled signal default handler
 * @resetted: resetted signal default handler
 * @tainted: tainted signal default handler
 * @variable_added: variable added signal default handler
 * @variable_removed: variable added signal default handler
 *
 * The CdnObject class
 *
 */
struct _CdnObjectClass
{
	/*< private >*/
	GObjectClass parent_class;

	/*< public >*/
	gboolean      (*compile)         (CdnObject              *object,
	                                  CdnCompileContext      *context,
	                                  CdnCompileErrorForward *error);

	void          (*reset)           (CdnObject *object);

	void          (*foreach_expression) (CdnObject                *object,
	                                     CdnForeachExpressionFunc  func,
	                                     gpointer                  userdata);

	gboolean      (*apply_template)  (CdnObject  *object,
	                                  CdnObject  *templ,
	                                  GError    **error);

	gboolean      (*unapply_template)  (CdnObject  *object,
	                                    CdnObject  *templ,
	                                    GError    **error);

	void          (*copy)            (CdnObject *object,
	                                  CdnObject *source);

	GType         (*get_copy_type)   (CdnObject *object);

	void          (*taint)           (CdnObject *object);

	GSList       *(*get_variables)  (CdnObject    *object);
	CdnVariable  *(*get_variable)    (CdnObject    *object,
	                                  const gchar  *name);

	gboolean      (*has_variable)    (CdnObject    *object,
	                                  const gchar  *name);

	gboolean      (*add_variable)    (CdnObject    *object,
	                                  CdnVariable  *variable,
	                                  GError      **error);

	gboolean      (*remove_variable) (CdnObject    *object,
	                                  const gchar  *name,
	                                  GError      **error);

	gboolean      (*verify_remove_variable) (CdnObject    *object,
	                                         const gchar  *name,
	                                         GError      **error);

	void          (*clear)           (CdnObject    *object);

	gboolean      (*equal)           (CdnObject    *first,
	                                  CdnObject    *last);

	CdnCompileContext *(*get_compile_context) (CdnObject         *object,
	                                           CdnCompileContext *context);

	/* signals */
	void          (*compiled)         (CdnObject   *object);
	void          (*resetted)         (CdnObject   *object);
	void          (*tainted)          (CdnObject   *object);

	void          (*copied)           (CdnObject   *object,
	                                   CdnObject   *copy);

	void          (*variable_added)   (CdnObject   *object,
	                                   CdnVariable *variable);
	void          (*variable_removed) (CdnObject   *object,
	                                   CdnVariable *variable);

	void          (*template_applied) (CdnObject   *object,
	                                   CdnObject   *templ);
	void          (*template_unapplied) (CdnObject *object,
	                                     CdnObject *templ);
};

GQuark cdn_object_error_quark (void);

GType cdn_object_get_type (void) G_GNUC_CONST;

CdnObject        *cdn_object_new               (const gchar *id);
CdnObject        *cdn_object_new_from_template (CdnObject *templ, GError **error);

const gchar      *cdn_object_get_id          (CdnObject   *object);
void              cdn_object_set_id          (CdnObject   *object,
                                              const gchar *id);

gboolean          cdn_object_add_variable    (CdnObject    *object,
                                              CdnVariable  *variable,
                                              GError      **error);
CdnVariable      *cdn_object_get_variable    (CdnObject   *object,
                                              const gchar *name);
gboolean          cdn_object_has_variable    (CdnObject   *object,
                                              const gchar *name);
gboolean          cdn_object_remove_variable (CdnObject    *object,
                                              const gchar  *name,
                                              GError      **error);

gboolean          cdn_object_verify_remove_variable (CdnObject    *object,
                                                     const gchar  *name,
                                                     GError      **error);

GSList           *cdn_object_get_variables  (CdnObject   *object);
CdnNodeForward  *cdn_object_get_parent      (CdnObject   *object);

gboolean          cdn_object_get_auto_imported (CdnObject    *object);
void              cdn_object_set_auto_imported (CdnObject    *object,
                                                gboolean      auto_imported);

/* evaluation */
void              cdn_object_reset          (CdnObject   *object);

void              cdn_object_foreach_expression (CdnObject                *object,
                                                 CdnForeachExpressionFunc  func,
                                                 gpointer                  userdata);

void              cdn_object_taint          (CdnObject   *object);

gboolean          cdn_object_is_compiled    (CdnObject   *object);

gboolean          cdn_object_compile        (CdnObject                          *object,
                                             CdnCompileContext                  *context,
                                             CdnCompileErrorForward *error);

gboolean          cdn_object_equal          (CdnObject *first,
                                             CdnObject *second);

void              cdn_object_clear          (CdnObject   *object);

const GSList     *cdn_object_get_applied_templates  (CdnObject   *object);
const GSList     *cdn_object_get_template_applies_to  (CdnObject   *object);

CdnObject        *cdn_object_copy           (CdnObject *object);

gboolean          cdn_object_apply_template (CdnObject  *object,
                                             CdnObject  *templ,
                                             GError    **error);

gboolean          cdn_object_unapply_template (CdnObject  *object,
                                               CdnObject  *templ,
                                               GError    **error);

CdnObject        *cdn_object_get_variable_template (CdnObject   *object,
                                                    CdnVariable *variable,
                                                    gboolean     match_full);

gchar            *cdn_object_get_full_id             (CdnObject *object);
gchar            *cdn_object_get_full_id_for_display (CdnObject *object);

gchar            *cdn_object_get_relative_id (CdnObject *object,
                                              CdnNodeForward *parent);

gchar            *cdn_object_get_relative_id_for_display (CdnObject *object,
                                                          CdnNodeForward *parent);

CdnCompileContext *cdn_object_get_compile_context (CdnObject         *object,
                                                   CdnCompileContext *context);

void             _cdn_object_set_parent     (CdnObject *object,
                                             CdnNodeForward *parent);

G_END_DECLS

#endif /* __CDN_OBJECT_H__ */
