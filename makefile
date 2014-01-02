CSC:=csc

all: sr4web 

sr4web: sr4web.scm 
	$(CSC) $^ -o $@


run: sr4web
	killall -q $<; nohup ./$< &
