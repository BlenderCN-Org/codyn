/*
 * cpg-property.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

#include <cpg-network/cpg-ref-counted.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

typedef enum
{
	CPG_PROPERTY_HINT_NONE = 0,
	CPG_PROPERTY_HINT_IN = 1 << 0,
	CPG_PROPERTY_HINT_OUT = 1 << 1,
	CPG_PROPERTY_HINT_ONCE = 1 << 2
} CpgPropertyHint;

typedef struct _CpgProperty CpgProperty;

/* forward declaration */
CPG_FORWARD_DECL (CpgObject);

GType			   cpg_property_get_type				(void);
CpgProperty 	  *cpg_property_new						(const gchar        *name, 
														 const gchar        *expression, 
														 gboolean            integrated, 
														 CPG_FORWARD_DECL (CpgObject)  *object);

CpgProperty       *_cpg_property_copy                   (CpgProperty        *property);
const gchar 	  *cpg_property_get_name				(CpgProperty        *property);

CPG_FORWARD_DECL (CpgObject) *cpg_property_get_object				(CpgProperty        *property);
void               _cpg_property_set_object             (CpgProperty        *property,
                                                         CPG_FORWARD_DECL (CpgObject)  *object);

gboolean		   cpg_property_get_integrated			(CpgProperty        *property);
void			   cpg_property_set_integrated			(CpgProperty		*property,
														 gboolean			 integrated);

CpgPropertyHint	   cpg_property_get_hint				(CpgProperty        *property);
void			   cpg_property_set_hint				(CpgProperty		*property,
														 CpgPropertyHint     hint);
void			   cpg_property_add_hint				(CpgProperty		*property,
														 CpgPropertyHint     hint);
void			   cpg_property_remove_hint				(CpgProperty		*property,
														 CpgPropertyHint     hint);
void 			   cpg_property_reset_cache				(CpgProperty		*property);

gdouble			   cpg_property_get_value				(CpgProperty        *property);
CpgExpression 	  *cpg_property_get_value_expression	(CpgProperty        *property);

void			   cpg_property_set_value				(CpgProperty        *property, 
														 gdouble             value);
void			   cpg_property_set_value_expression	(CpgProperty        *property, 
														 const gchar 		*expression);

gboolean		   cpg_property_equal					(CpgProperty        *property,
														 CpgProperty        *other);

void			   _cpg_property_set_update				(CpgProperty	    *property,
														 gdouble             value);
gdouble			   _cpg_property_get_update				(CpgProperty	    *property);

void 			   _cpg_property_use                    (CpgProperty        *property);
gboolean           _cpg_property_unuse                  (CpgProperty        *property);
guint	            cpg_property_get_used               (CpgProperty        *property);

G_END_DECLS

#endif /* __CPG_PROPERTY_H__ */
