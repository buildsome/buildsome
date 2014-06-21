echo "x.mk says hello"

ifneq (${FLAG_y}, enable)
echo y is disabled
else
echo y is enabled
endif
