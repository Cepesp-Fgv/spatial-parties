#!/bin/bash
rm -rf /srv/shiny-server/spatial-parties/static/cache/*.gz > /var/log/codedeploy/spatial_parties_clear_cache.log 2>&1
