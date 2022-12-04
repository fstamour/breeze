
test:
	scripts/test.sh

doc:
	scripts/doc.sh

demo:
	scripts/demo/build-docker-image.sh
	scripts/demo/run-demo-recorder.sh

.PHONY: \
	test \
	doc \
	demo
