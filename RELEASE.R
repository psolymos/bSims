# Compile README.md using latest version of package
devtools::build_readme()
# Rd
# devtools::document()
# Check/update URLS
urlchecker::url_check()

# Check spelling
dict <- hunspell::dictionary('en_US')
devtools::spell_check()
spelling::update_wordlist()

# local checks
devtools::check()

# install
devtools::install()

