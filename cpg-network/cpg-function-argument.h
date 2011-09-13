/*
 * cpg-function-argument.h
 * This file is part of cpg-network
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

#ifndef __CPG_FUNCTION_ARGUMENT_H__
#define __CPG_FUNCTION_ARGUMENT_H__

#include <glib-object.h>
#include <cpg-network/cpg-property.h>

G_BEGIN_DECLS

#define CPG_TYPE_FUNCTION_ARGUMENT		(cpg_function_argument_get_type ())
#define CPG_FUNCTION_ARGUMENT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgument))
#define CPG_FUNCTION_ARGUMENT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgument const))
#define CPG_FUNCTION_ARGUMENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgumentClass))
#define CPG_IS_FUNCTION_ARGUMENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_FUNCTION_ARGUMENT))
#define CPG_IS_FUNCTION_ARGUMENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_FUNCTION_ARGUMENT))
#define CPG_FUNCTION_ARGUMENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgumentClass))

typedef struct _CpgFunctionArgument		CpgFunctionArgument;
typedef struct _CpgFunctionArgumentClass	CpgFunctionArgumentClass;
typedef struct _CpgFunctionArgumentPrivate	CpgFunctionArgumentPrivate;

struct _CpgFunctionArgument
{
	/*< private >*/
	GInitiallyUnowned parent;

	CpgFunctionArgumentPrivate *priv;
};

struct _CpgFunctionArgumentClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;

	/*< public >*/

	/* signals */
	gboolean (*invalidate_name) (CpgFunctionArgument *argument,
	                             const gchar         *name);
};

GType                cpg_function_argument_get_type           (void) G_GNUC_CONST;

CpgFunctionArgument *cpg_function_argument_new                (const gchar         *name,
                                                               CpgExpression       *expression,
                                                               gboolean             isexplicit);

CpgFunctionArgument *cpg_function_argument_copy               (CpgFunctionArgument *argument);

const gchar         *cpg_function_argument_get_name           (CpgFunctionArgument *argument);
gboolean             cpg_function_argument_set_name           (CpgFunctionArgument *argument,
                                                               const gchar         *name);

gboolean             cpg_function_argument_get_optional       (CpgFunctionArgument *argument);
void                 cpg_function_argument_set_optional       (CpgFunctionArgument *argument,
                                                               gboolean             optional);

CpgExpression       *cpg_function_argument_get_default_value  (CpgFunctionArgument *argument);
void                 cpg_function_argument_set_default_value  (CpgFunctionArgument *argument,
                                                               CpgExpression       *expression);

gboolean             cpg_function_argument_get_explicit       (CpgFunctionArgument *argument);
void                 cpg_function_argument_set_explicit       (CpgFunctionArgument *argument,
                                                               gboolean             isexplicit);

void                 _cpg_function_argument_set_property      (CpgFunctionArgument *argument,
                                                               CpgProperty         *property);

CpgProperty         *_cpg_function_argument_get_property      (CpgFunctionArgument *argument);

G_END_DECLS

#endif /* __CPG_FUNCTION_ARGUMENT_H__ */
