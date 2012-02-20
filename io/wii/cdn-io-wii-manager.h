#ifndef __CDN_IO_WII_MANAGER_H__
#define __CDN_IO_WII_MANAGER_H__

#include <glib-object.h>
#include <gmodule.h>
#include <cwiid.h>
#include <codyn/cdn-object.h>
#include <codyn/integrators/cdn-integrator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INPUT_WII_MANAGER		(cdn_io_wii_manager_get_type ())
#define CDN_IO_WII_MANAGER(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_WII_MANAGER, CdnIoWiiManager))
#define CDN_IO_WII_MANAGER_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT_WII_MANAGER, CdnIoWiiManager const))
#define CDN_IO_WII_MANAGER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INPUT_WII_MANAGER, CdnIoWiiManagerClass))
#define CDN_IS_INPUT_WII_MANAGER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INPUT_WII_MANAGER))
#define CDN_IS_INPUT_WII_MANAGER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INPUT_WII_MANAGER))
#define CDN_IO_WII_MANAGER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INPUT_WII_MANAGER, CdnIoWiiManagerClass))

typedef struct _CdnIoWiiManager			CdnIoWiiManager;
typedef struct _CdnIoWiiManagerClass		CdnIoWiiManagerClass;
typedef struct _CdnIoWiiManagerPrivate		CdnIoWiiManagerPrivate;

struct _CdnIoWiiManager
{
	/*< private >*/
	GObject parent;

	CdnIoWiiManagerPrivate *priv;
};

struct _CdnIoWiiManagerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType                cdn_io_wii_manager_get_type        (void) G_GNUC_CONST;
void                 cdn_io_wii_manager_register        (GTypeModule        *module);

CdnIoWiiManager  *cdn_io_wii_manager_new             (void);

gchar              **cdn_io_wii_manager_get_remotes     (CdnIoWiiManager *manager);
gchar const         *cdn_io_wii_manager_get_remote_name (CdnIoWiiManager *manager,
                                                            gchar const        *addr);

gboolean             cdn_io_wii_manager_bind_remote  (CdnIoWiiManager *manager,
                                                         guint               deviceid,
                                                         CdnObject          *object);

void                 cdn_io_wii_manager_reset           (CdnIoWiiManager *manager);
void                 cdn_io_wii_manager_update          (CdnIoWiiManager *manager,
                                                            CdnIntegrator      *integrator);

G_END_DECLS

#endif /* __CDN_IO_WII_MANAGER_H__ */
