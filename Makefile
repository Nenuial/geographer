document:
		R -e 'devtools::document()'

build:
		R -e 'devtools::build()'

install:
		R -e 'pak::local_install(upgrade = FALSE)'
