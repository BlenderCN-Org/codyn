/*
 * cdn-variable.h
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

#ifndef __CDN_VARIABLE_H__
#define __CDN_VARIABLE_H__

#include <codyn/cdn-expression.h>
#include <codyn/cdn-utils.h>
#include <codyn/cdn-modifiable.h>
#include <codyn/cdn-usable.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_VARIABLE            (cdn_variable_get_type ())
#define CDN_VARIABLE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_VARIABLE, CdnVariable))
#define CDN_VARIABLE_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_VARIABLE, CdnVariable const))
#define CDN_VARIABLE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_VARIABLE, CdnVariableClass))
#define CDN_IS_VARIABLE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_VARIABLE))
#define CDN_IS_VARIABLE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_VARIABLE))
#define CDN_VARIABLE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_VARIABLE, CdnVariableClass))

typedef struct _CdnVariable            CdnVariable;
typedef struct _CdnVariableClass    CdnVariableClass;
typedef struct _CdnVariablePrivate    CdnVariablePrivate;

/**
 * CdnVariableFlags:
 * @CDN_VARIABLE_FLAG_NONE: none
 * @CDN_VARIABLE_FLAG_INTEGRATED: integrated
 * @CDN_VARIABLE_FLAG_IN: in
 * @CDN_VARIABLE_FLAG_OUT: out
 * @CDN_VARIABLE_FLAG_ONCE: once
 * @CDN_VARIABLE_FLAG_INOUT: convenience for CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT
 *
 * Property flags.
 *
 */
typedef enum
{
	CDN_VARIABLE_FLAG_NONE = 0,
	CDN_VARIABLE_FLAG_INTEGRATED = 1 << 0,
	CDN_VARIABLE_FLAG_IN = 1 << 1,
	CDN_VARIABLE_FLAG_OUT = 1 << 2,
	CDN_VARIABLE_FLAG_ONCE = 1 << 3,

	CDN_VARIABLE_FLAG_INOUT = CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT
} CdnVariableFlags;

struct _CdnVariable
{
	/*< private >*/
	GInitiallyUnowned parent;

	CdnVariablePrivate *priv;
};

struct _CdnVariableClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;

	/*< public >*/

	/* signals */
	gboolean (*invalidate_name) (CdnVariable *property,
	                             const gchar *name);

	void (*expression_changed) (CdnVariable      *property,
	                            CdnExpression    *expression);
	void (*flags_changed)      (CdnVariable      *property,
	                            CdnVariableFlags  flags);
};

GType cdn_variable_get_type (void) G_GNUC_CONST;

CdnVariable       *cdn_variable_new                     (const gchar      *name,
                                                         CdnExpression    *expression,
                                                         CdnVariableFlags  flags);

const gchar       *cdn_variable_get_name                (CdnVariable        *property);
gboolean           cdn_variable_set_name                (CdnVariable        *property,
                                                         const gchar        *name);

CdnObjectForward *cdn_variable_get_object              (CdnVariable        *property);

gboolean           cdn_variable_get_integrated          (CdnVariable        *property);
void               cdn_variable_set_integrated          (CdnVariable        *property,
                                                         gboolean            integrated);

CdnVariableFlags   cdn_variable_get_flags               (CdnVariable        *property);
void               cdn_variable_set_flags               (CdnVariable        *property,
                                                         CdnVariableFlags    flags);
void               cdn_variable_add_flags               (CdnVariable        *property,
                                                         CdnVariableFlags    flags);
void               cdn_variable_remove_flags            (CdnVariable        *property,
                                                         CdnVariableFlags    flags);
void               cdn_variable_reset                   (CdnVariable        *property);

gdouble            cdn_variable_get_value               (CdnVariable        *property);
gdouble const     *cdn_variable_get_values              (CdnVariable        *property,
                                                         gint               *numr,
                                                         gint               *numc);

CdnExpression     *cdn_variable_get_expression          (CdnVariable        *property);

void               cdn_variable_set_value               (CdnVariable        *property,
                                                         gdouble             value);

void               cdn_variable_set_values              (CdnVariable        *property,
                                                         gdouble const      *values,
                                                         gint                numr,
                                                         gint                numc);

void               cdn_variable_set_expression          (CdnVariable        *property,
                                                         CdnExpression      *expression);

void               cdn_variable_set_constraint          (CdnVariable       *property,
                                                         CdnExpression     *expression);

CdnExpression     *cdn_variable_get_constraint          (CdnVariable       *property);

gboolean           cdn_variable_equal                   (CdnVariable        *property,
                                                         CdnVariable        *other,
                                                         gboolean            asstring);

void               cdn_variable_set_update              (CdnVariable        *property,
                                                         gdouble const      *values);

void               cdn_variable_clear_update            (CdnVariable        *property);

void               cdn_variable_set_update_value        (CdnVariable        *property,
                                                         gdouble             value,
                                                         gint                numr,
                                                         gint                numc);

gdouble           *cdn_variable_get_update              (CdnVariable        *property,
                                                         gint               *numr,
                                                         gint               *numc);

gchar             *cdn_variable_flags_to_string         (CdnVariableFlags    add_flags,
                                                         CdnVariableFlags    remove_flags);

void               cdn_variable_flags_from_string       (const gchar        *flags,
                                                         CdnVariableFlags   *add_flags,
                                                         CdnVariableFlags   *remove_flags);

gchar             *cdn_variable_get_full_name             (CdnVariable        *property);
gchar             *cdn_variable_get_full_name_for_display (CdnVariable        *property);

CdnVariable       *cdn_variable_copy                    (CdnVariable        *property);

void               _cdn_variable_set_object             (CdnVariable      *property,
                                                         CdnObjectForward *object,
                                                         gboolean          notify);

GSList            *cdn_variable_get_actions             (CdnVariable *property);

CdnVariable       *cdn_variable_get_integral            (CdnVariable *property);
CdnVariable       *cdn_variable_get_derivative          (CdnVariable *property);

void               cdn_variable_set_derivative          (CdnVariable *property,
                                                         CdnVariable *diffprop);

gboolean           cdn_variable_compile                 (CdnVariable              *property,
                                                         CdnCompileErrorForward   *error);

G_END_DECLS

#endif /* __CDN_VARIABLE_H__ */
