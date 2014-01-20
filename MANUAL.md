Manual
======

TODO :)

Some useful information about the starting of the servive
---------------------------------------------------------

After installation of the bead, one should select a directory where the bead
server can create its environment. The environment consist of the data directory,
the users.json file, the logging directory and the dictionary directory for the i18n
translation files.

To create an example translation file run the CreateBeadLanguageFile, which creates
a template, that one need to fill up with the current translation. The translation needs
to be added into the "dic" subdirectory of the bead's currently run directory, from
where the server reads up the translations. If a translation is valid it is printed
on the console at startup.

