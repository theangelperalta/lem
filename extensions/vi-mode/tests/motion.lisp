(defpackage :lem-vi-mode/tests/motion
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/motion)

(in-readtable :interpol-syntax)

(deftest vi-forward-char
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bcdef\n")
      (cmd "l")
      (ok (buf= #?"a[b]cdef\n"))
      (cmd "3l")
      (ok (buf= #?"abcd[e]f\n"))
      (cmd "10l")
      (ok (buf= #?"abcde[f]\n")))))

(deftest vi-forward-word-end
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc()def#ghi jkl ()\n")
      (cmd "e")
      (ok (buf= #?"ab[c]()def#ghi jkl ()\n"))
      (cmd "e")
      (ok (buf= #?"abc([)]def#ghi jkl ()\n"))
      (cmd "e")
      (ok (buf= #?"abc()de[f]#ghi jkl ()\n"))
      (cmd "e")
      (ok (buf= #?"abc()def[#]ghi jkl ()\n"))
      (cmd "e")
      (ok (buf= #?"abc()def#gh[i] jkl ()\n"))
      (cmd "2e")
      (ok (buf= #?"abc()def#ghi jkl ([)]\n"))
      (cmd "e")
      (ok (buf= #?"abc()def#ghi jkl ()\n[]"))
      (cmd "10e")
      (ok (buf= #?"abc()def#ghi jkl ()\n[]")))
    (with-vi-buffer (#?"[a]bc def () \n\t # ghi\n")
      (cmd "e")
      (ok (buf= #?"ab[c] def () \n\t # ghi\n"))
      (cmd "e")
      (ok (buf= #?"abc de[f] () \n\t # ghi\n"))
      (cmd "e")
      (ok (buf= #?"abc def ([)] \n\t # ghi\n"))
      (cmd "e")
      (ok (buf= #?"abc def () \n\t [#] ghi\n"))
      (cmd "e")
      (ok (buf= #?"abc def () \n\t # gh[i]\n")))
    (with-vi-buffer (#?"abc [ ]\t def\n\n ghi")
      (cmd "e")
      (ok (buf= #?"abc  \t de[f]\n\n ghi"))
      (cmd "l")
      (cmd "e")
      (ok (buf= #?"abc  \t def\n\n gh[i]")))
    (with-vi-buffer (#?"a[b]cd efg hij klm nop")
      (cmd "de")
      (ok (buf= #?"a[ ]efg hij klm nop"))
      (cmd "de")
      (ok (buf= #?"a[ ]hij klm nop"))
      (cmd "d2e")
      (ok (buf= #?"a[ ]nop")))))

(deftest vi-forward-word-end-broad
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc-def (ghi jkl) # \n\t m")
      (cmd "E")
      (ok (buf= #?"abc-de[f] (ghi jkl) # \n\t m"))
      (cmd "E")
      (ok (buf= #?"abc-def (gh[i] jkl) # \n\t m"))
      (cmd "E")
      (ok (buf= #?"abc-def (ghi jkl[)] # \n\t m"))
      (cmd "E")
      (ok (buf= #?"abc-def (ghi jkl) [#] \n\t m"))
      (cmd "E")
      (ok (buf= #?"abc-def (ghi jkl) # \n\t [m]"))
      (cmd "E")
      (ok (buf= #?"abc-def (ghi jkl) # \n\t m[]"))
      (cmd "gg")
      (cmd "3E")
      (ok (buf= #?"abc-def (ghi jkl[)] # \n\t m")))
    (with-vi-buffer ("ab[c]-def ghi # jkl")
      (cmd "dE")
      (ok (buf= "ab[ ]ghi # jkl"))
      (cmd "d2E")
      (ok (buf= "ab[ ]jkl")))))

(deftest vi-backward-word-begin
  (with-fake-interface ()
    (with-vi-buffer (#?"abc()def#ghi jkl ()[\n]")
      (cmd "b")
      (ok (buf= #?"abc()def#ghi jkl [(])\n"))
      (cmd "2b")
      (ok (buf= #?"abc()def#[g]hi jkl ()\n"))
      (cmd "b")
      (ok (buf= #?"abc()def[#]ghi jkl ()\n"))
      (cmd "b")
      (ok (buf= #?"abc()[d]ef#ghi jkl ()\n"))
      (cmd "b")
      (ok (buf= #?"abc[(])def#ghi jkl ()\n"))
      (cmd "b")
      (ok (buf= #?"[a]bc()def#ghi jkl ()\n"))
      (cmd "10b")
      (ok (buf= #?"[a]bc()def#ghi jkl ()\n")))
    (with-vi-buffer (#?"abc def () \n\t # ghi[\n]")
      (cmd "b")
      (ok (buf= #?"abc def () \n\t # [g]hi\n"))
      (cmd "b")
      (ok (buf= #?"abc def () \n\t [#] ghi\n"))
      (cmd "b")
      (ok (buf= #?"abc def [(]) \n\t # ghi\n"))
      (cmd "b")
      (ok (buf= #?"abc [d]ef () \n\t # ghi\n"))
      (cmd "b")
      (ok (buf= #?"[a]bc def () \n\t # ghi\n")))
    (with-vi-buffer (#?"abc\n\n  \t de[f]")
      (cmd "b")
      (ok (buf= #?"abc\n\n  \t [d]ef"))
      (cmd "h")
      (cmd "b")
      (ok (buf= #?"[a]bc\n\n  \t def")))
    (with-vi-buffer ("abc # def-g[h]i")
      (cmd "db")
      (ok (buf= "abc # def-[h]i"))
      (cmd "d2b")
      (ok (buf= "abc # [h]i")))))

(deftest vi-backward-word-begin-broad
  (with-fake-interface ()
    (with-vi-buffer (#?"a \t\n # (bcd efg) hij-kl[m]")
      (cmd "B")
      (ok (buf= #?"a \t\n # (bcd efg) [h]ij-klm"))
      (cmd "B")
      (ok (buf= #?"a \t\n # (bcd [e]fg) hij-klm"))
      (cmd "B")
      (ok (buf= #?"a \t\n # [(]bcd efg) hij-klm"))
      (cmd "B")
      (ok (buf= #?"a \t\n [#] (bcd efg) hij-klm"))
      (cmd "B")
      (ok (buf= #?"[a] \t\n # (bcd efg) hij-klm"))
      (cmd "G$")
      (cmd "3B")
      (ok (buf= #?"a \t\n # [(]bcd efg) hij-klm")))
    (with-vi-buffer ("jkl # ghi abc-[d]ef")
      (cmd "dB")
      (ok (buf= "jkl # ghi [d]ef"))
      (cmd "d2B")
      (ok (buf= "jkl [d]ef")))))

(deftest vi-forward-word-begin
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc()def#ghi jkl ()\n")
      (cmd "w")
      (ok (buf= #?"abc[(])def#ghi jkl ()\n"))
      (cmd "w")
      (ok (buf= #?"abc()[d]ef#ghi jkl ()\n"))
      (cmd "w")
      (ok (buf= #?"abc()def[#]ghi jkl ()\n"))
      (cmd "w")
      (ok (buf= #?"abc()def#[g]hi jkl ()\n"))
      (cmd "2w")
      (ok (buf= #?"abc()def#ghi jkl [(])\n"))
      (cmd "10w")
      (ok (buf= #?"abc()def#ghi jkl ()\n[]")))
    (with-vi-buffer (#?"[a]bc def () \n\t # ghi\n")
      (cmd "w")
      (ok (buf= #?"abc [d]ef () \n\t # ghi\n"))
      (cmd "w")
      (ok (buf= #?"abc def [(]) \n\t # ghi\n"))
      (cmd "w")
      (ok (buf= #?"abc def () \n\t [#] ghi\n"))
      (cmd "w")
      (ok (buf= #?"abc def () \n\t # [g]hi\n"))
      (cmd "w")
      (ok (buf= #?"abc def () \n\t # ghi\n[]")))
    (with-vi-buffer (#?"abc [ ]\t def\n\n ghi")
      (cmd "w")
      (ok (buf= #?"abc  \t [d]ef\n\n ghi"))
      (cmd "3l")
      (cmd "w")
      (ok (buf= #?"abc  \t def\n\n [g]hi")))
    (with-vi-buffer (#?"a[b]cd efg hij klm nop")
      (cmd "dw")
      (ok (buf= #?"a[e]fg hij klm nop"))
      (cmd "dw")
      (ok (buf= #?"a[h]ij klm nop"))
      (cmd "d2w")
      (ok (buf= #?"a[n]op")))))

(deftest vi-forward-word-begin-broad
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc-def (ghi jkl) # \n\t m")
      (cmd "W")
      (ok (buf= #?"abc-def [(]ghi jkl) # \n\t m"))
      (cmd "W")
      (ok (buf= #?"abc-def (ghi [j]kl) # \n\t m"))
      (cmd "W")
      (ok (buf= #?"abc-def (ghi jkl) [#] \n\t m"))
      (cmd "W")
      (ok (buf= #?"abc-def (ghi jkl) # \n\t [m]"))
      (cmd "W")
      (ok (buf= #?"abc-def (ghi jkl) # \n\t m[]"))
      (cmd "gg")
      (cmd "3W")
      (ok (buf= #?"abc-def (ghi jkl) [#] \n\t m")))
    (with-vi-buffer ("ab[c]-def ghi # jkl")
      (cmd "dW")
      (ok (buf= "ab[g]hi # jkl"))
      (cmd "d2W")
      (ok (buf= "ab[j]kl")))))

(deftest vi-find-char
  (with-fake-interface ()
    (with-vi-buffer ("[f]oo-bar-Baz")
      (cmd "f-")
      (ok (buf= "foo[-]bar-Baz"))
      (cmd "fB")
      (ok (buf= "foo-bar-[B]az"))
      (cmd "^2f-")
      (ok (buf= "foo-bar[-]Baz"))
      (cmd "^3fa")
      (ok (buf= "[f]oo-bar-Baz"))
      (cmd ";")
      (ok (buf= "foo-b[a]r-Baz"))
      (cmd "^2;")
      (ok (buf= "foo-bar-B[a]z")))))

(deftest vi-find-char-before
  (with-fake-interface ()
    (with-vi-buffer ("[f]oo-bar-Baz")
      (cmd "t-")
      (ok (buf= "fo[o]-bar-Baz"))
      (cmd "tB")
      (ok (buf= "foo-bar[-]Baz"))
      (cmd "^2t-")
      (ok (buf= "foo-ba[r]-Baz"))
      (cmd "^3ta")
      (ok (buf= "[f]oo-bar-Baz"))
      (cmd ";")
      (ok (buf= "foo-[b]ar-Baz"))
      (cmd "^2;")
      (ok (buf= "foo-bar-[B]az")))))

(deftest vi-find-char-backward
  (with-fake-interface ()
    (with-vi-buffer ("foo-bar-Ba[z]")
      (cmd "F-")
      (ok (buf= "foo-bar[-]Baz"))
      (cmd "$Fb")
      (ok (buf= "foo-[b]ar-Baz"))
      (cmd "$2F-")
      (ok (buf= "foo[-]bar-Baz"))
      (cmd "$3Fa")
      (ok (buf= "foo-bar-Ba[z]"))
      (cmd ";")
      (ok (buf= "foo-bar-B[a]z"))
      (cmd "$2;")
      (ok (buf= "foo-b[a]r-Baz")))))

(deftest vi-find-char-backward-after
  (with-fake-interface ()
    (with-vi-buffer ("foo-bar-Baz[z]")
      (cmd "T-")
      (ok (buf= "foo-bar-[B]azz"))
      (cmd "$Tb")
      (ok (buf= "foo-b[a]r-Bazz"))
      (cmd "$2T-")
      (ok (buf= "foo-[b]ar-Bazz"))
      (cmd "$3Ta")
      (ok (buf= "foo-bar-Baz[z]"))
      (cmd ";")
      (ok (buf= "foo-bar-Ba[z]z"))
      (cmd "$2;")
      (ok (buf= "foo-ba[r]-Bazz")))))