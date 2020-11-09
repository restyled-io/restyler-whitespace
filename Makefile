VERSION = $(shell sed '/^version: \(.*\)$$/!d; s//v\1/' package.yaml)

.PHONY: release
release:
	[ -n "$(VERSION)" ]
	docker build --tag restyled/restyler-whitespace:$(VERSION) .
	docker push restyled/restyler-whitespace:$(VERSION)
	git tag --sign --message $(VERSION) $(VERSION)
	git push --follow-tags
	@echo
	@echo "image: restyled/restyler-whitespace:$(VERSION)"
