%%------------------------------------------------------------------------------
%% @doc otpshim
%%
%% OTP "shim" for configuring/invoking "up" the OTP stack
%%
%% @copyright 2019 Jeremey L Barrett <jlb@rot26.com>
%%------------------------------------------------------------------------------
-module(otpshim).

-behaviour(application).

%% Profile API
-export([get_profile/0, apply_profile/1]).

%% Application callbacks
-export([start/2, stop/1]).

%% Ceremonial API
-export([start/0, stop/0]).
-export([get_env/1, get_env/2, set_env/2]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

-spec start(StartType :: atom(), StartArgs :: [ term() ]) ->
    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    %% do the work of otpshim
    apply_profile(get_profile()),

    %% start the supervisor for OTP happiness
    otpshim_sup:start_link().


-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.


%%==============================================================================
%% Implementation
%%==============================================================================

-spec get_profile() -> atom().
get_profile() ->
    case os_profile() of
        undefined ->
            get_env(profile, undefined);
        X -> X
    end.

-spec apply_profile(ProfileName :: atom()) -> ok.
apply_profile(undefined) ->
    ok;
apply_profile(ProfileName) ->
    do_profile(ProfileName, proplists:get_value(ProfileName, get_env(profiles, []))).


do_profile(_ProfileName, undefined) ->
    false;
do_profile(_ProfileName, []) ->
    ok;
do_profile(ProfileName, [Elem|T]) ->
    do_profile_elem(ProfileName, Elem),
    do_profile(ProfileName, T).

do_profile_elem(_ProfileName, {app, App}) ->
    application:load(App);
do_profile_elem(_ProfileName, {env, App, Key, Val}) ->
    application:load(App),
    application:set_env(App, Key, Val);
do_profile_elem(_ProfileName, {func, {M, F, A}}) ->
    erlang:apply(M, F, A);
do_profile_elem(_, _) ->
    erlang:error(badarg).

os_profile() ->
    case os:getenv("OTPSHIM_PROFILE") of
        "" -> undefined;
        L when is_list(L) ->
            list_to_atom(L);
        _ -> undefined
    end.


%%==============================================================================
%% Ceremonial API functions
%%==============================================================================

-spec start() -> ok.
start() ->
    {ok, _Started} = application:ensure_all_started(otpshim, permanent),
    ok.


-spec stop() -> ok.
stop() ->
    application:stop(otpshim).


-spec get_env(Key :: atom()) -> Value :: term() | 'undefined'.
get_env(Key) ->
    get_env(Key, 'undefined').


-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(otpshim, Key) of
        {ok, X} -> X;
        'undefined' -> Default
    end.


-spec set_env(Key :: atom(), Value :: any()) -> ok.
set_env(Key, Value) ->
    application:set_env(otpshim, Key, Value).
