-module(pop).
-behaviour(gen_server).
-export([start_link/0, start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 init/1,
	 terminate/2]).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start() ->
    start([{population_limit, 15}]).
start(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

start_link() ->
    start_link([{population_limit, 1}]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).



stop() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {population, population_limit}).

init(Args) ->

    error_logger:info_msg("Starting with ~p~n", [Args]),
    PopLimit = proplists:get_value(population_limit, Args), % TODO: Error on missing
    State = #state { population=populate(PopLimit, []),
		     population_limit=PopLimit},
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call({}, _From, State) ->
    {reply, [], State};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast({result, Genotype, Phenotype, Result}, State) ->
    io:format("result from ~p:~p -> ~p~n", [Genotype, Phenotype, Result]),
    {noreply, State};
handle_cast(CastEvent, State) ->
    io:format("Unknonw cast: ~p~n", [CastEvent]),
    {noreply, State}.


handle_info({'EXIT', Pid, normal}, State) ->
    %% Remove the dead member
    TrimmedPop = lists:delete(Pid, State#state.population),
    NewPopulation = populate(State#state.population_limit,
			     TrimmedPop),
    {noreply, State#state{ population=NewPopulation}}.

code_change(oldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

populate(Limit, Population) when length(Population) =:= Limit ->
    Population;
populate(Limit, Population) ->
    process_flag(trap_exit, true),
    Genotype = create_genotype(),
    Phenotype = create_phenotype(Genotype),
    Pid = spawn_link(Phenotype),
    populate(Limit, [Pid|Population]).
			  
create_genotype() ->
    random:uniform(100).

create_phenotype(Genotype) ->
    %% Just let the controller know of the result
    fun() ->
	    Result = Genotype,
	    timer:sleep(1000),
	    gen_server:cast(?MODULE, {result, Genotype, self(), Result})
    end.
