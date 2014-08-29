.PHONY: default
default: \
  a.out \
  b.out \
  c.out \
  d.out \
  e.out \
  f.out \
  g.out \
  h.out \
  i.out \
  j.out \
  k.out \
  l.out \
  m.out \
  n.out \
  o.out \
  p.out \
  q.out \
  r.out \
  s.out \
  t.out \
  u.out \
  v.out \
  w.out \
  x.out \
  y.out \
  z.out \
  fail

%.in:
	echo B.$@ >> $@
	echo A.$@ >> $@
	echo B.$@ >> $@

%.out: %.in
	for a in `seq 1 100`
	do
	    cat $^ | sort -u > $@
	done
	exit 2

fail:
	sleep 0.5
	exit 1
