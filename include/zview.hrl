-record(zview_tpl, {id, app, module, name, filename, source, last_modified}).
-record(zview_cfg, {app, template_root}).

-define(ZVIEW_TAB, zview_templates).
-define(ZVIEW_CFG, zview_config).
