pwd

eval $(opam config env)

cd "${APPVEYOR_BUILD_FOLDER}"

opam install -y cppo

jbuilder build rincewind.exe