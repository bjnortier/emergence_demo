-module(pop).
-behaviour(gen_server).
-export([start_link/0, start/1, start/0, stop/0]).

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
    start([]).
start(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

start_link() ->
    start_link([]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    catch(gen_server:call(?MODULE, stop)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {population, population_limit, organism_module}).

init(Args) ->
    process_flag(trap_exit, true),
    PopLimit = proplists:get_value(population_limit, Args, 5), 
    OrganismModule = proplists:get_value(organism_module, Args, hello_world), 
    State = #state { population = seed(OrganismModule, PopLimit, []),
		     population_limit = PopLimit,
		     organism_module = OrganismModule},
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call({}, _From, State) ->
    {reply, [], State};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast({result, Genotype, Phenotype, Result}, State) ->
    io:format("result from ~p:~p -> ~p~n", [Genotype, Phenotype, Result]),
    State1 = State#state{ population = 
			  lists:keyreplace(Phenotype, 2,
					   State#state.population,
					   {Genotype, Phenotype, Result})},
    {noreply, try_procreate(State1)};
handle_cast(CastEvent, State) ->
    io:format("Unknown cast: ~p~n", [CastEvent]),
    {noreply, State}.


handle_info({'EXIT', _DeadPid, normal}, State) ->
    {noreply, try_procreate(State)}.

code_change(oldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seed(_OrganismModule, Limit, Population) when length(Population) =:= Limit ->
    Population;
seed(OrganismModule, Limit, Population) ->
    Genotype = create_genotype(OrganismModule),
    %% Add the organism with undefined fitness
    seed(OrganismModule, Limit, birth(Genotype, OrganismModule, Population)).


birth(Genotype, OrganismModule, Population) ->
    Phenotype = create_phenotype(OrganismModule, Genotype),
    Pid = spawn_link(Phenotype),
    [{Genotype, Pid, undefined}|Population].

try_procreate(State) ->
    %% Individuals with fitness 
    OriginalPopulation = State#state.population,
    WithFitness= lists:filter(fun({_, _, Fitness}) ->
				      Fitness /= undefined
			      end,
			      OriginalPopulation),

    Deceased = lists:sort(fun({_, _, FitnessA}, {_, _, FitnessB}) ->
				  FitnessA =< FitnessB
			  end,
			  lists:filter(fun({_, Pid, _}) ->
			   		       not is_process_alive(Pid)
			   	       end,
			   	       OriginalPopulation)),

    %% If there are two or more individuals with a valid fitness
    %% Then we bury the dead individual with the lowest fitness
    if
	(length(WithFitness) > 2) and (length(Deceased) >= 1) -> 
	    procreate(State, WithFitness, Deceased);
	true -> 
	    State
    end.



procreate(State, WithFitness, Deceased) ->
    OriginalPopulation = State#state.population,
    OrganismModule = State#state.organism_module,
    LeastFitDeceased = hd(Deceased), 
    PopuationWithSpace = lists:delete(LeastFitDeceased, OriginalPopulation),

    SortedByFitness = lists:sort(fun({_, _, FitnessA}, {_, _, FitnessB}) ->
				FitnessA > FitnessB
			end,
			WithFitness),
    
    %% TODO: Convert this to a distribution function
    {GenotypeA, _, _} = lists:nth(1, SortedByFitness),
    {GenotypeB, _, _} = lists:nth(2, SortedByFitness),

    Genotype1 = OrganismModule:combine(GenotypeA, GenotypeB),
    ShouldMutate = random:uniform() > 0.9,
    Genotype2 = if
		    ShouldMutate -> OrganismModule:mutate(Genotype1);
		    true -> Genotype1
		end,
    %%io:format("~p + ~p -> ~p~n", [GenotypeA, GenotypeB, Genotype2]),
    
    State#state{ population = birth(Genotype2, 
				    OrganismModule,
				    PopuationWithSpace)}.
				    

create_genotype(OrganismModule) ->
    OrganismModule:initial().


create_phenotype(OrganismModule, Genotype) ->
    %% Just let the controller know of the result
    Phenotype = OrganismModule:to_phenotype(Genotype),
    fun() ->
 	    Result = OrganismModule:fitness(Phenotype),
 	    gen_server:cast(?MODULE, {result, Genotype, self(), Result}),
 	    %% Keeps on living so it can procreate
 	    timer:sleep(100)
    end.
