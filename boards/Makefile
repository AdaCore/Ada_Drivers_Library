all:
	for gpr in *.gpr; do \
	  echo Building $$gpr; \
	  gprbuild -q -j0 -XPLATFORM_BUILD=Debug -P $$gpr -p; \
	  gprbuild -q -j0 -XPLATFORM_BUILD=Production -P $$gpr -p; \
	done
