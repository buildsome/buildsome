FLAG_x ?= disable
FLAG_y ?= enable
ifeq (${FLAG_x}, enable)
echo YESX BEFORE
include "x.mk"
echo YESX AFTER
else
echo NOTX BEFORE
include "notx.mk"
echo NOTX AFTER
endif
.PHONY: default
default:
