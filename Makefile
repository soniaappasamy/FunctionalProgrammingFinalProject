client:
	corebuild -pkgs oUnit,yojson,str,async,core,lablgtk2 main_client.byte
	./main_client.byte

server:
	corebuild -pkgs oUnit,yojson,str,async,core main_server.byte
	./main_server.byte
	
test:
	corebuild -pkgs oUnit,yojson,str,unix,async,core test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	corebuild -clean
	
zip:
	zip finalprojectsrc.zip *.ml{,i,y,l}