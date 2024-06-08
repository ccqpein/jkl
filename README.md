Run Shell Programs in Common Lisp the Lisp Way

## Why

I prefer to call the shell command `curl` like this:

```lisp
(jkl-cmd:curl "google.com" :v t :jkl-error nil)
```

rather than:

```lisp
(sb-ext:run-program "curl" '("google.com" "-v") :search t :output *standard-output* :error nil)
```

## Installation

You will need the normal Common Lisp toolchain, such as SBCL/Quicklisp. The only thing to be aware of is that I am using the `str:match` macro, which is in a newer version than the Quicklisp version (2024-06-04). Therefore, you may need to download [cl-str](https://github.com/vindarel/cl-str) to the `local-projects` directory of Quicklisp.

## Usage

*Check the `cmds` folder to see the implemented commands.*

To run `curl` to access Google:

```lisp
(jkl-cmd:curl "www.google.com")
```

Just like in the shell:

```shell
curl "www.google.com"
```

### Keywords Explained

I will use the `cmds/curl` command as an example again.

```lisp
(jkl-cmd:curl :help t)
```

is equivalent to:

```shell
curl --help
```

In actuality, it equals `curl --help t` because `curl`'s help accepts a value (category).

> -h, --help \<category\> Get help for commands

So that's how the keyword works in `jkl-cmd`. The `:help` keyword translates to `--help`, and the keyword value, `t`, translates to `t` in the shell.

If I do:

```lisp
(jkl-cmd:curl :help "all")
```

it will show all the help options for `curl`.

I can use the short option keyword to do the same thing:

```lisp
(jkl-cmd:curl :h1 "all")
```

Wait a second, why is it `h1` rather than `h`? Common Lisp's keywords in lambda lists are case-insensitive. This means that `:h` and `:H` are the same when I call `(jkl-cmd:curl :h t)`. However, they are different in `curl`:

> -H, --header \<header/@file\> Pass custom header(s) to server
> -h, --help \<category\> Get help for commands

So, when the command instance is generated, the second `h/H` will become `:h1`. This only applies to short options, of course. Before you use the command, you can check if the keyword is correct or not by using `jkl:get-options` and `jkl:list-options`.

### Flag Option

For flag options, such as the version of `curl` (`-V`, `--version`), the option doesn't accept any value after it. However, we have to call it with `t` in `jkl`. For example:

```lisp
(jkl-cmd:curl :v1 t)
```

### Creating a New Command

If you have your own command-line application and you want to call it inside your Common Lisp application through `jkl`, you can create your own command inside the `cmds` folder (within the `jkl-cmd` package).

If the options of the command-line app you made are similar to those of `curl` (e.g., `-d, --data <data> HTTP POST data`), for example, if you make a command-line app using [clingon](https://github.com/dnaeon/clingon), you can use `jkl-options:option4` and `jkl:make-new-command` to create the new command:

```lisp
;; in cmds/my-app.lisp
(in-package :jkl-cmd)

(defparameter *app-help* "option example
  -n, --id <INT>        the id of quiz
  -o, --output <VALUE>  output file
  ")

(defparameter *app*
  (make-new-command "app"
                    (loop for line in (read-line-content *app-help*)
                          for opt = (parse-option-from-help 'option4 line)
                          when opt
                            collect opt)))

;;; Run the app command with args
(defun app (&rest args)
  (apply #'run *app* args))

;; For others
(export '(app *app*))
```

Then you can run your app with the following syntax:

```lisp
(jkl-cmd:app :n 123 :output "output.txt")
```

### Sub-command

If a command-line app has subcommands, we can add the `:subcommand` keyword when we call `make-new-command`.

```lisp
(jkl:make-new-command
  "top"
  (mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option1 line))
          '("--capath <dir> CA directory to verify peer against"
            "-E, --cert <certificate[:password]> Client certificate file and password"))
  :subcommand `(("a" ,(mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option1 line))
                              '("--capath <dir> CA directory to verify peer against"
                                "-E, --cert <certificate[:password]> Client certificate file and password"))
                 :subcommand (("b" ,(mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option2 line))
                                            '("-r, --recursive specify recursive download")))))))
```

The `:subcommand` option receives a list of `make-new-command` lambda lists. Then, when you call:

```lisp
(top :e "cert" "a" :e "cert" "b" :r t)
```

you are effectively calling `top -E cert a -E cert b -r` in the shell.

### Helper Functions

**List All Options of a Command**

Given the keyword sequence feature, you might need to check which is the correct keyword for an option.

```lisp
;; lambda list of list-options ((comm command) &key short-option-start-with long-option-start-with with-key)
(jkl:list-options jkl-cmd:*curl* :with-key t :short-option-start-with "h")
```

This will output:

```lisp
(("H"
  (T short-option: H
   long-option: header
   argument: header/@file
   description: Pass custom header(s) to server
))
 ("HEADER"
  (NIL short-option: H
   long-option: header
   argument: header/@file
   description: Pass custom header(s) to server
))
 ("H1"
  (T short-option: h
   long-option: help
   argument: category
   description: Get help for commands
))
 ("HELP"
  (NIL short-option: h
   long-option: help
   argument: category
   description: Get help for commands
))
)
```

`jkl:list-options` will list all options of `jkl-cmd:*curl*` whose short options start with "h". In `curl`, this means `header` and `help`. So the keywords for `header` are `:h`, `:H`, `:HEADER`, and `:header`. Keywords for `curl`'s help are `:h1`, `:H1`, `:HELP`, and `:help`. 

**Check If Keywords Are Correct/Get Options by Keywords**

```lisp
;; get-options lambda list is ((comm command) &rest keys)
(jkl:get-options jkl-cmd:*curl* :v :h :h1 :help)
```

This will output:

```lisp
;; ((short-flag option)...)
((T short-option: v
   long-option: verbose
   argument: 
   description: Make the operation more talkative
)
 (T short-option: H
   long-option: header
   argument: header/@file
   description: Pass custom header(s) to server
)
 (T short-option: h
   long-option: help
   argument: category
   description: Get help for commands
)
 (NIL short-option: h
   long-option: help
   argument: category
   description: Get help for commands
))
```

**See How Options Look in Shell**

`jkl:gen-options` will show how the arguments look in the shell. I use it for debugging.

```lisp
(jkl:gen-options jkl-cmd:*curl* :v1 t :H "hello" :H1 "?")
;; Output: ("-V" "-H" "hello" "-h" "?")
```

## TODO

+ Add more Common Lisp implementations. 
  Currently, I only use SBCL, so I have only implemented the SBCL run function (see the `run` function in `core.lisp`).
+ Add more commands to the `cmds` folder.
