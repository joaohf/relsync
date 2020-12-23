# relsync

[![Build Status](https://github.com/joaohf/relsync/workflows/CI/badge.svg)](https://github.com/joaohf/relsync/actions?query=branch%3Amaster+workflow%3A"CI")

Relsync synchronizes the contents of a local Erlang/OTP release with a
remote node. It is similar to rsync in that it attempts to only copy
files that have been added or changed, but also reloads changed .beam
files and supports Erlang pre and post synchronization scripts.

The intended use case is for copying cross-compiled Erlang releases to
their target hardware. It probably can be used in other scenarios, but
other tools like [sync](https://github.com/rustyio/sync) may be easier
to use. The advantage to using `relsync` is that it copies ports over
as well and let's you add scripts to perform custom reloading or run
other code needed to update the remote filesystem. In fact, it
synchronizes almost everything that's safe to synchronize. Shared
libraries (.so) are one of the main exceptions. Ports can be syncronized
so long as they are stopped in the presync part of the script.

Here's an example usage to synchronize the release in the `_rel` directory
with a remote node on a Beaglebone.

    relsync --mode erl --destination-node testnode@beaglebone --hooks relsync_hooks.erl --cookie beagle --sname relsync

## Building

Building is similar to other Erlang projects. Make sure `rebar3` is in
your path.

### escript

    git clone https://github.com/fhunleth/relsync.git
    cd relsync
    rebar3 as release escriptize
    _build/release/bin/relsync -h

To install, copy the `relsync` output to anywhere convenient in your `$PATH`.

## Testing

Tests are implemented using the common test framework. And executing them is like this:

    rebar3 ct --sname node

## escript usage

```
Usage: relsync [-d [<destnode>]] [-p [<destpath>]] [-q [<destrwpath>]]
               [-l [<localpath>]] [-H <hooks>]
               [-E [<exclude_system_libs>]] [-c [<cookie>]] [-s <sname>]
               [-n <name>] [-M [<mode>]] [-P <port>] [-u <user>] [-h]

  -d, --destination-node     Destination node [default: node@other]
  -p, --destination-path     Path to release on the destination (Can be on 
                             a read-only filesystem) [default: /srv/erlang]
  -q, --destination-rw-path  Path to writable location on the destination 
                             (empty if not needed) [default: ]
  -l, --local-path           Path to local release [default: ./_rel]
  -H, --hooks                Erlang module containing hooks to run on the 
                             destination
  -E, --exclude-system-libs  Do not synchronize system libs [default: true]
  -c, --cookie               Erlang magic cookie to use [default: cookie]
  -s, --sname                Short name for the local node
  -n, --name                 Long name for the local node
  -M, --mode                 Method to synchronize release using ssh or 
                             erlang distributed protocol [default: ssh]
  -P, --port                 ssh port to use
  -u, --user                 ssh username
  -h, --help                 Show help usage
```

`relsync` supports two operation modes when transferring files:

* _erl_: this is the default mode and uses Erlang Distribution Protocol in order to transfer
files to the target hardware
* _ssh_: transfer files using the [SSH FTP](http://erlang.org/doc/man/ssh_sftp.html) protocol and uses a [ssh subsystem](http://erlang.org/doc/apps/ssh/using_ssh.html#creating-a-subsystem) helper application on the target in order to manage the synchronization phase

Each operation mode has a set of specific parameters like this:

* ssh:
  * `-d, --destination-node`: target IP address
  * `-P --port`: target SSH port number
  * `-u --user`: user name to use, default: _relsync_
  
* erl:
  * `-c, --cookie`: Erlang magic cookie
  * `-s, --sname` or `-n, --name`: node short name or long name  

The rest of the parameters are common for both operation modes.

## rebar3 plugin

Add `relsync` plugin as project_plugins:

    {project_plugins, [relsync]}.

The command line options can be used to configure the relsync plugin:

    {relsync, [
        {mode, "ssh"},
        {port, 2222},
        {user, relsync}
    ]}

After configuring rebar3, the rebar3 subcommand relsync is available and works like this:

    rebar3 relsync --destination-node 192.168.7.2 --local-path _build/{profile}/rel/{release-name}/lib/ --destination-path <target destination path>


## Target requirements

When using the `erl` mode `relsync` pushes the synchronization code over 
to the target, not much is needed. The target should have the `kernel`,
`stdlib`, and `crypto` applications available.

If the mode `ssh` is active, then the target should have the `kernel`,
`stdlib`, `crypto`, `ssh` and `asn1` applications available. Also the release application
should include the `relsyncd` application which controls the file transfer and code load.

If the target stores the Erlang applications on read-only filesystems, that's
ok. Use the `-q` parameter to specify a writable filesystem to use and `relsync`
will put the new files there and update the Erlang VM's search path to pick the
new files up. The Erlang VM search path will no longer point to the original
read-only filesystem location. Symlinks are used to point back to unmodified
files.

### Preparing target release when using ssh mode

In order to prepare a target release with supports code updates using relsync ssh mode,
the rebar.config file should have two additional configurations:

* a specific profile which adds the relsyncd dependency application, example:
```
{profiles, [
            {devtest, [
                {deps, [relsyncd]}
            ]}
           ]
}.
```
* and a specific release that includes the relsyncd application, example:
```
{relx, [
    {release, {'<release name>', "0.1.0"}, [<your application>, relsyncd], []}
    ]
}.
```

After configuring the rebar3 with the respective profile and release, then calling rebar3 to create
a release, like this:

    rebar3 as devtest release -n <release name>

The above command uses the profile _devtest_ which brings the relsyncd application and creates the release _\<release name\>_ that includes the relsyncd application.

## Hooks

Relsync will look for the module specified by `--hooks` parameter and if it
isn't found, it will look for a `.erl` file of the same name and use it. The
code in the module is run on the destination node.

The following example hooks kill one of the ports so that it can be updated.
It also remounts the filesystem so that it is writable and can receive the updates.

```erlang
-module(relsync_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Got a presync~n"),

    % Stop the application so that any active ports are
    % exited. This is needed or relsync won't be able to update
    % the binary.
    application:stop(myapp).

postsync() ->
    io:format("Got a postsync~n"),

    % Start the app back up
    application:start(myapp).
```

## Development

relsync is split up in three parts:

* relsync_lib: common functions
* relsyncd: SSH subsystem helper application
* relsync: a hybrid escript and rebar3 plugin which has the main command line interface

All of these parts share a umbrella project which generates three set of artifacts:

* relsync escript
* rebar3 relsync plugin
* hex packages

For code format, [erlfmt](https://github.com/WhatsApp/erlfmt) is used to keep the formatting code consistent.

## Acknowledgment

* Frank Hunleth who did the initial design and release the first working relsync tool version

## License

[Apache 2 License](https://github.com/joaohf/relsync/LICENSE)

## TODO

1. Clean up error messages

2. Handle updating multiple boards at once. Some code is there, but
   the command line doesn't support it

3. Add more docs
