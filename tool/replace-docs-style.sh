#!/bin/sh
# Replaces the entirety of the style tag within each HTML file in /docs with a link to a stylesheet.
sed -i '/<style/,/<\/style>/{/<style/{s/.*/    <link rel="stylesheet" type="text\/css" href="res\/css\/main.css">/;n};d};
    s/starcanopy.outcome/outcome/' docs/*.html