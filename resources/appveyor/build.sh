pwd

eval $(opam config env)

cd "${APPVEYOR_BUILD_FOLDER}"

opam install -y jbuilder cppo

jbuilder build rincewind.exe