pwd

eval $(opam config env)

cd "${APPVEYOR_BUILD_FOLDER}"

jbuilder build rincewind.exe