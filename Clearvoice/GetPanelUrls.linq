<Query Kind="Expression">
</Query>

from ou in Org_panel_urls
where ou.Panel_url.StartsWith("localhost")
select new{ou.Org_id, ou.Default_culture, ou.Panel_url, ou.Panel_theme}