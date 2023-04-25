# Support for International Languages

Ledger is a currency and commodity agnostic accounting tool;
in this spirit Ledger strives to embrace more languages to
provide its utility to those who are less fluent or familiar
with the English language.

For details on the used custom gettext macros
see [`src/system.hh.in`](http://git.ledger-cli.org/ledger/tree/master/src/system.hh.in)

For details regarding [gettext] and its use please refer to the
[manual](https://www.gnu.org/software/gettext/manual/gettext.html)


## Updating An Existing Language

Overtime the translatable strings in Ledger will change and with them the
translations need to be changed too.

Building the translations from a clean build folder and without any
local changes will modify the `.po`-files in the `po/` directory, e.g. `po/it.po`:

`rm -rf build && cmake -S. -Bbuild && cmake --build build --target translations`

Change the translation `.po`-files accordingly and
[issue a pull request](http://git.ledger-cli.org/ledger/pulls) to get the changes
reviewed and approved


## Adding Translations For A New Language

➊  Add a `LOCALE` environment variable and set it to a
[ISO 639-1 2-letter language code](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes),
e.g. `it` for Italian
➋  Ensure there are no local changes in the repository
➌  Generate ledger.pot file from a clean build.
➍  Generate a new translation file name `it.po` based on the translatable strings in `ledger.pot`
➎ Remove the temporary `LOCALE` environment variable

```
export LOCALE=it  # ➊
git status -s #  ➋
rm -rf build && cmake -S. -Bbuild && cmake --build build --target translations #  ➌
msginit --locale ${LOCALE} --input build/po/ledger.pot --output-file po/${LOCALE}.po  # ➍
unset LOCALE  # ➎
```

Add the translation for the new language to the`.po`-files and then
add these to git (`git add po/*.po`), finally
[issue a pull request](http://git.ledger-cli.org/ledger/pulls)
to get the new translation files reviewed and added.



[gettext]: https://www.gnu.org/software/gettext/
