: ''
TIMEFORMAT="%R sec"
time clang sussy.cpp -std=c++17 -g -luser32.lib -Wno-deprecated-declarations -Wno-null-dereference -o sussy.exe
if [ $? -eq 0 ]; then
    time ./sussy.exe comp test.sus
else
    echo build fail
fi
#'
