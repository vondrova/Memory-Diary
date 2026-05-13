# Memory Diary

A personal web application for recording and rediscovering life memories, built with a Haskell backend and an Elm frontend.

## Functional Requirements

- Creating, editing, and deleting memory entries (each entry has a title, date and time range from–to including hours, and optionally a description, location, tags, and photos)
- Browsing memories in a timeline view (chronological) and a calendar view (monthly overview with highlighted days)
- Searching and filtering by tags, location, date range, and full-text search in title and description (filters can be combined)
- Statistics — total time recorded together, most used tags, most visited locations, activity heatmap by month/day (like GitLab)
- "On This Day" — showing memories from previous years that fall on today's date (like Google Photos)
- Anniversaries and birthdays — storing recurring important dates with a countdown to the next occurrence
- The application will run via Docker
