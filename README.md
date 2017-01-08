egitd
=====

Original code by Tom Preston-Werner <tom@github.com>

egitd is an Erlang git-daemon implementation that provides a more flexible,
scalable, and loggable way to serve public git repositories.

**Development Status: Experimental**

This software was in production use at github.com for a short time until it
became obvious that the communications model was flawed. To be specific,
if the upload-pack takes a long time to respond (for big repos), either the
timeouts have to be increased to unreasonable values (slowing the entire
transfer down), or some connections will timeout and fail.

The above problem has actually been solved by a rewrite Vagabond has done
 as an exercise in how to improve the performance of erlang
applications. egitd is now only marginally slower than git-daemon on the
same repo, and does not suffer the timeout issues mentioned above even on
the large repos like gentoo's portage where github was having problems.

You can see Vagabond's rewrite notes here: http://andrew.hijacked.us/by_keyword/328/egit
