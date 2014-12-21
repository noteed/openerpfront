all: .stack_haxr_touched

.stack_haxr_touched:
	docker build -t images.reesd.com/reesd/stack-haxr images/stack-haxr
	touch $@
