PACKAGE_NAME=ergoemacs-keybindings-$(VERSION)

all:
	@echo Usage:
	@echo "  make package VERSION=x.x.x"

package:
	mkdir $(PACKAGE_NAME)
	cat ergoemacs-mode.el \
		| sed -e "s/^;; Version:[ 0-9.]*/;; Version: $(VERSION)/" \
		      -e "s/ergoemacs-mode-version \"[ 0-9.]*\"/ergoemacs-mode-version \"$(VERSION)\"/" \
		> ergoemacs-mode.tmp
	mv ergoemacs-mode.tmp ergoemacs-mode.el
	cp *.el *.txt $(PACKAGE_NAME)
	-rm $(PACKAGE_NAME).zip
	zip -9 $(PACKAGE_NAME).zip $(PACKAGE_NAME)/*.*
	rm -fr $(PACKAGE_NAME)
