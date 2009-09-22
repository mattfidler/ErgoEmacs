PACKAGE_NAME=ergoemacs-keybindings-$(VERSION)

all:
	@echo Usage:
	@echo "  make package VERSION=x.x.x"

package:
	mkdir $(PACKAGE_NAME)
	cp *.el *.txt $(PACKAGE_NAME)
	cat ergoemacs-mode.el \
		| sed -e "s/^;; Version:[ 0-9.]*/;; Version: $(VERSION)/" \
		      -e "s/ergoemacs-mode-version \"5.1\"/ergoemacs-mode-version \"$(VERSION)\"/" \
		> $(PACKAGE_NAME)/ergoemacs-mode.el
	-rm $(PACKAGE_NAME).tar.gz
	tar czf $(PACKAGE_NAME).tar.gz $(PACKAGE_NAME)
	rm -fr $(PACKAGE_NAME)
