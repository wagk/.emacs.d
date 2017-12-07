;;; Compiled snippets and support files for `elixir-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'elixir-mode
                     '(("unless" "unless ${1:condition} do\n  $0\nend\n" "unless" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/unless" nil nil)
                       ("test" "test \"$1\" do\n  $0\nend\n" "test" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/test" nil nil)
                       ("pry" "require IEx; IEx.pry\n" "pry" nil
                        ("debug")
                        nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/pry" nil nil)
                       ("mdoc" "@moduledoc \"\"\"\n$0\n\"\"\"\n" "moduledoc" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/mdoc" nil nil)
                       ("iop" "IO.puts(\"$1 #{inspect $1}\")$0\n" "iop" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/iop" nil nil)
                       ("io" "IO.puts(\"$1\")$0\n" "io" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/io" nil nil)
                       ("ife" "if ${1:condition} do\n  $2\nelse\n  $3\nend\n" "if-else" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/ife" nil nil)
                       ("if" "if ${1:condition} do\n  $0\nend\n" "if" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/if" nil nil)
                       ("hinfo" "def handle_info($1, state) do\n  $0\n  {:noreply, state}\nend\n" "hinfo" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/hinfo" nil nil)
                       ("hcast" "def handle_cast($1, state) do\n  $0\n  {:noreply, state}\nend\n" "hcast" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/hcast" nil nil)
                       ("hcall" "def handle_call($1, _from, state) do\n  reply = $0\n  {:reply, reply, state}\nend\n" "hcall" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/hcall" nil nil)
                       ("for" "for ${2:x} <- ${1:enum} do\n  $2$0\nend\n" "for" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/for" nil nil)
                       ("fn" "fn ${1:x} -> $1$0 end" "fn" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/fn" nil nil)
                       ("doc" "@doc \"\"\"\n$0\n\"\"\"\n" "doc" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/doc" nil nil)
                       ("do" "do\n  $0\nend\n" "do" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/do" nil nil)
                       ("defp" "defp $1 do\n  $0\nend\n" "defp" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/defp" nil nil)
                       ("defmodule" "defmodule $1 do\n  $0\nend\n" "defmodule" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/defmodule" nil nil)
                       ("defmacrop" "defmacrop $1 do\n  $0\nend\n" "defmacrop" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/defmacrop" nil nil)
                       ("defmacro" "defmacro $1 do\n  $0\nend\n" "defmacro" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/defmacro" nil nil)
                       ("def" "def ${1:function}${2:(${3:args})} do\n  $0\nend\n" "def" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/def" nil nil)
                       ("cond" "cond do\n  $0\nend\n" "cond" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/cond" nil nil)
                       ("cast" "GenServer.cast(${1:__MODULE__}, $0)\n" "cast" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/cast" nil nil)
                       ("case" "case $1 do\n  $0\nend\n" "case" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/case" nil nil)
                       ("call" "GenServer.call(${1:__MODULE__}, $0)\n" "call" nil nil nil "/home/pangt/.emacs.d/elpa/yasnippet-20170717.1946/snippets/elixir-mode/call" nil nil)))


;;; Do not edit! File generated at Wed Jul 19 11:34:17 2017
