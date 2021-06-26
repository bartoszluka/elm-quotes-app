OUTJS = elm.js
OUTMIN = elm.min.js
 
all: js
js:
	elm make src/Main.elm --output=$(OUTJS)

min: optimize
	uglifyjs $(OUTJS) --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $(OUTMIN)
optimize: 
	elm make src/Main.elm --optimize --output=$(OUTJS)

clean: 
	@echo cleaning
	del $(OUTJS) $(OUTMIN)


.PHONY: clean min