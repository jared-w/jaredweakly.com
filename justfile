ghcid:
	ghcid --reload=./app --reload=./site -T Main.main

clean-shake:
	watchexec --exts hs -- rm -rf ./shake

serve:
	serve dist

watch:
	npx concurrently -r -k "just ghcid" "just clean-shake" "just serve"
