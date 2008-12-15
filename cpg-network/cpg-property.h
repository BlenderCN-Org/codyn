#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

struct _CpgExpression;
struct _CpgObject;

typedef struct 
{
	char *name;
	struct _CpgExpression *value;
	double update;
	struct _CpgExpression *initial;
	char integrated;
	
	struct _CpgObject *object;
} CpgProperty;

CpgProperty 	*cpg_property_new			(char const *name, char const *expression, char integrated, struct _CpgObject *object);
void			 cpg_property_free			(CpgProperty *property);

char const 		*cpg_property_name			(CpgProperty *property);

struct _CpgObject *cpg_property_object		(CpgProperty *property);

/* set numerics, for expressions use #cpg_network_set_value and friends */
void			 cpg_property_set_value		(CpgProperty *property, double value);
void			 cpg_property_set_initial 	(CpgProperty *property, double value);

/* get numerics, for expressions use #cpg_network_value and friends */
double			 cpg_property_value			(CpgProperty *property);
double			 cpg_property_initial		(CpgProperty *property);

#endif /* __CPG_PROPERTY_H__ */
