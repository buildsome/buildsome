.PHONY: \
  test1_ok \
  test2_bad \
  test2_bad_dir \
  test2_bad_file \
  test3_bad_file \
  test3_bad_dir \
  test3_bad_dir.stat

test1_ok:
	mkdir /tmp/test1.dir
	echo hello >/tmp/test1.file
	rmdir /tmp/test1.dir
	rm /tmp/test1.file

test1_bad:
	mkdir /tmp/test1.dir
	echo hello >/tmp/test1.file

test2_bad: test2_bad_dir test2_bad_file

foodir:
	mkdir $@

test2_bad_dir: foodir
	mkdir $< # allowed because it's a no-op to re-mkdir
	rmdir $< # not allowed!

foo:
	echo foo > $@

test2_bad_file: foo
	echo bar >> $<

test3_bad_file:
	echo hi > /tmp/test3.file
	cat test3_file_scratch_accessor
	rm /tmp/test3.file

test3_file_scratch_accessor:
	cat /tmp/test3.file
	echo > $@ ; rm $@

test3_bad_dir:
	mkdir /tmp/test3.dir
	cat test3_dir_scratch_accessor
	rmdir /tmp/test3.dir

test3_dir_scratch_accessor:
	cat /tmp/test3.dir/foo
	echo > $@ ; rm $@

test3_bad_dir.stat:
	mkdir /tmp/test3.dir
	cat test3_dir_scratch_accessor.stat
	rmdir /tmp/test3.dir

test3_dir_scratch_accessor.stat:
	stat /tmp/test3.dir
	echo > $@ ; rm $@
