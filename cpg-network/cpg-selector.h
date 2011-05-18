#ifndef __CPG_SELECTOR_H__
#define __CPG_SELECTOR_H__

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-link.h>

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

typedef struct _CpgExpansion CpgExpansion;
typedef struct _CpgSelection CpgSelection;

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

gchar const  *cpg_selector_as_string         (CpgSelector            *selector);

CpgSelector  *cpg_selector_expand            (CpgSelector            *selector,
                                              GSList                 *expansions);

GSList       *cpg_selector_get_expansions    (CpgSelector            *selector,
                                              gpointer                matched);

void          cpg_selector_add               (CpgSelector            *selector,
                                              gchar const            *identifier);

void          cpg_selector_add_pseudo        (CpgSelector            *selector,
                                              gchar const            *pseudo,
                                              gchar const * const    *arguments);

void          cpg_selector_add_regex         (CpgSelector            *selector,
                                              gchar const            *regex);

GSList       *cpg_selector_select            (CpgSelector            *selector,
                                              CpgObject              *parent);

GSList       *cpg_selector_select_link_to    (CpgSelector            *selector,
                                              CpgObject              *parent,
                                              CpgObject              *from);

GSList       *cpg_selector_select_links      (CpgSelector            *selector,
                                              CpgObject              *parent);

GSList       *cpg_selector_select_states     (CpgSelector            *selector,
                                              CpgObject              *parent);

GSList       *cpg_selector_select_properties (CpgSelector            *selector,
                                              CpgObject              *parent);

CpgExpansion *cpg_expansion_new              (gchar const * const    *items);
CpgExpansion *cpg_expansion_copy             (CpgExpansion           *id);

gint          cpg_expansion_num              (CpgExpansion           *id);

gchar const  *cpg_expansion_get              (CpgExpansion           *id,
                                              gint                    idx);

void          cpg_expansion_add              (CpgExpansion           *id,
                                              gchar const            *item);

void          cpg_expansion_set              (CpgExpansion           *id,
                                              gint                    idx,
                                              gchar const            *val);

void          cpg_expansion_free             (CpgExpansion           *id);

gchar        *cpg_expansions_expand          (GSList                 *expansions,
                                              gchar const            *s);

gchar        *cpg_expansion_expand           (CpgExpansion           *id,
                                              gchar const            *s);

gchar       **cpg_expansion_expand_all       (CpgExpansion           *id,
                                              gchar const * const    *s);

CpgSelection *cpg_selection_new              (gpointer                object,
                                              GSList                 *expansions);

void          cpg_selection_free             (CpgSelection           *selection);
CpgSelection *cpg_selection_copy             (CpgSelection           *selection);

CpgObject    *cpg_selection_get_object       (CpgSelection           *selection);
CpgProperty  *cpg_selection_get_property     (CpgSelection           *selection);
GSList       *cpg_selection_get_expansions   (CpgSelection           *selection);

G_END_DECLS

#endif /* __CPG_SELECTOR_H__ */
