---
title: "README"
output: html_document
---

# RAD Watershed Prioritization Tool

## API Token Setup

Getting data from the Riverscapes Data Exchange requires a Riverscapes API token, stored as an environment variable.

To set up your token:

1.  Create a new .txt file named .Renviron and paste RIVERSCAPES_TOKEN=your_actual_token_here into the first line
2.  Log in to <https://data.riverscapes.net>
3.  Open DevTools (F12) and search "Bearer"
4.  Copy the Authorization (everything after "Bearer")
5.  In the .Renviron file you created, replace your_actual_token_here with the copied Authorization
6.  Save the .Renviron file and restart R

*Note: because tokens expire, you may need to obtain a new token periodically.*

## Get Data

To get the required data run the get_data.R file.
