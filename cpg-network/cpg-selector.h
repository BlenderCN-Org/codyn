#ifndef __CPG_SELECTOR_H__
#define __CPG_SELECTOR_H__

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-link.h>
#include <cpg-network/cpg-embedded-string.h>
#include <cpg-network/cpg-selection.h>

G_BEGIN_DECLS

#define CPG_TYPE_SELECTOR		(cpg_selector_get_type ())
#define CPG_SELECTOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTOR, CpgSelector))
#define CPG_SELECTOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTOR, CpgSelector const))
#define CPG_SELECTOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_SELECTOR, CpgSelectorClass))
#define CPG_IS_SELECTOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_SELECTOR))
#define CPG_IS_SELECTOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_SELECTOR))
#define CPG_SELECTOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_SELECTOR, CpgSelectorClass))

typedef struct _CpgSelector		CpgSelector;
typedef struct _CpgSelectorClass	CpgSelectorClass;
typedef struct _CpgSelectorPrivate	CpgSelectorPrivate;

struct _CpgSelector
{
	GObject parent;

	CpgSelectorPrivate *priv;
};

struct _CpgSelectorClass
{
	GObjectClass parent_class;
};

GType         cpg_selector_get_type          (void) G_GNUC_CONST;

CpgSelector  *cpg_selector_new               (void);
CpgSelector  *cpg_selector_parse             (gchar const            *ptr,
                                              GError                **error);

CpgSelector  *cpg_selector_copy              (CpgSelector            *selector);

gchar const  *cpg_selector_as_string         (CpgSelector            *selector);

void          cpg_selector_add               (CpgSelector            *selector,
                                              CpgEmbeddedString      *identifier);

void          cpg_selector_add_pseudo        (CpgSelector            *selector,
                                              CpgEmbeddedString      *pseudo,
                                              GSList                 *arguments);

void          cpg_selector_add_regex         (CpgSelector            *selector,
                                              CpgEmbeddedString      *regex);

GSList       *cpg_selector_select            (CpgSelector            *selector,
                                              CpgObject              *parent,
                                              CpgEmbeddedContext     *context);

GSList       *cpg_selector_select_links      (CpgSelector            *selector,
                                              CpgObject              *parent,
                                              CpgEmbeddedContext     *context);

GSList       *cpg_selector_select_states     (CpgSelector            *selector,
                                              CpgObject              *parent,
                                              CpgEmbeddedContext     *context);

GSList       *cpg_selector_select_properties (CpgSelector            *selector,
                                              CpgObject              *parent,
                                              CpgEmbeddedContext     *context);

G_END_DECLS

#endif /* __CPG_SELECTOR_H__ */
