# error\_emacs
============

This application will help you to fix the errors really fast. error_emacs will
detect crashes in the shell and redirect emacs to that file and mark
the line for you. The rest is (still) up to you.

.erlang
=======

I currently start it from my .erlang file but only when running the shell
like this:

```erlang
Args = init:get_arguments(),
WantShell = case proplists:get_value(noshell, Args) of
                [] -> false;
        true -> false;
        _ -> true
    end,
if WantShell ->
    application:start(error_emacs);
true ->
    ok
end.
```

.emacs
======

You must run emacs server in order to use error_emacs. So please add

```elisp
;;
;; Emacs server - used by error_emacs.erl among other things
;;
(server-start)
```

emacsclient
===========

The program emacsclient must be found in the path, on Mac OS X you may
have to add

```sh
PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
```

To override the default emacsclient, but only if you are using the Emacs.app

