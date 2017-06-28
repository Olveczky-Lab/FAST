package main

import (
	"net/http"
	"net/url"
	"os"
	"strconv"
	"syscall"
	"runtime"
	"io"
	"fmt"
)

func readhandler(w http.ResponseWriter, r *http.Request) {
	v := r.URL.Query()
	fmt.Printf("%v\n",v)
	fname, _ := url.QueryUnescape(v["FileName"][0])
	f, _ := os.Open(fname)
	defer f.Close()
	o, _ := strconv.ParseInt(v["Offset"][0], 10, 64)
	f.Seek(o,0)
	n, _ := strconv.ParseInt(v["NumBytes"][0],10,64)
	if (n < 0) {
		fi, _ := f.Stat()
		n = fi.Size() - o
	}
	io.CopyN(w,f,n)
}

func sizehandler(w http.ResponseWriter, r *http.Request) {
	v := r.URL.Query()
	fmt.Printf("%v\n",v)
	fname, _ := url.QueryUnescape(v["FileName"][0])
	f, _ := os.Open(fname)
	defer f.Close()
	fi, _ := f.Stat()
	n := fi.Size()
	fmt.Fprintf(w,"%v",n)
}

func writehandler(w http.ResponseWriter, r *http.Request) {
	v := r.URL.Query()
	fmt.Printf("%v\n",v)
	fname, _ := url.QueryUnescape(v["FileName"][0])
	o, _ := strconv.ParseInt(v["Offset"][0], 10, 64)
	f, _ := os.OpenFile(fname,syscall.O_CREAT|syscall.O_WRONLY,0644)
	defer f.Close()
	f.Seek(o,0)
	io.Copy(f,r.Body)
}

func main() {
	runtime.GOMAXPROCS(8)
	http.HandleFunc("/read", readhandler)
	http.HandleFunc("/write", writehandler)
	http.HandleFunc("/size",sizehandler)
	http.ListenAndServe(":" + os.Args[1], nil)
}
