otpshim
=====

otpshim is a trivial library designed to be inserted at the very "bottom" of your OTP
application stack. It is frequently very useful to adjust local/dev behaviour of
applications, or to perform special local/dev initialization, before those applications
are started automagically, usually by `rebar3 shell`.

otpshim simply performs a set of function calls or application configurations at
startup on behalf of OTP apps higher in the stack. otpshim will `application:load`
each application before it applies configuration (relying on the OTP behaviour of
only loading an app once).

otpshim accepts a list of profiles in its Erlang environment, and the specific profile
to use can be specified via the profile Erlang environment variable or more like, the
OTPSHIM_PROFILE OS environment variable.

## Configuration

In your shell.config:

```erlang
[
%% ...
 {otpshim,
  [
   {profiles,
    [
     {local, [
              {env, some_app, somekey, someval},
              {env, another_app, foo, bar},
              {app, someapptoload},
              {func, {somemod, somefunc, []}},
              {func, {someothermod, someotherfunc, []}}
             ]}
    ]}
  ]}
%% ...
].
```

Then in your OS shell:

```
export OTPSHIM_PROFILE=local
```

And then in your app.src for your OTP app:

```erlang
{application, myapp,
 [{description, "My App"},
  {vsn, git},
  {modules, []},
  {registered, [
      myapp_srv,
      myapp_sup
   ]},
  {applications,
   [kernel,
    stdlib,

    otpshim,     % this goes before anything else

    jiffy,       % for example
    gun          % for example

    %% ...
   ]},
  {mod, {myapp_app, []}},
  {env,[]}
 ]}.
```

## How to report defects

Open github issues.

