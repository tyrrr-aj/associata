-module(dbg_counter).

-export([create/0, add_inference/3, add_stimulations/4, print_report/1]).

-record(state, {
    inferences=#{}, 
    total_vn_stimulations=#{}, 
    total_on_stimulations=0, 
    vn_stimulations_by_inference=#{},
    on_stimulations_by_inference=#{}
}).


create() -> 
    ok. % spawn(fun() -> process_events(#state{}) end).


add_inference(InferenceType, InferenceId, DbgCounter) -> 
    ok. % DbgCounter ! {inference, InferenceType, InferenceId}.


add_stimulations(on, Count, InferenceId, DbgCounter) -> 
    ok; % DbgCounter ! {on, Count, InferenceId};

add_stimulations({vn, VNGName}, Count, InferenceId, DbgCounter) -> 
    ok. % DbgCounter ! {vn, VNGName, Count, InferenceId}.


print_report(DbgCounter) -> 
    ok.
    % receive after 5000 -> ok end,
    % DbgCounter ! {print_report, self()},
    % receive
    %     report_printed -> ok
    % end.



process_events(#state{
    inferences=Inferences,
    total_vn_stimulations=TotalVNStimulations, 
    total_on_stimulations=TotalONStimulations,
    vn_stimulations_by_inference=VNStimulationsByInference,
    on_stimulations_by_inference=ONStimulationsByInference
} = State) ->

    receive
        {inference, InferenceType, InferenceId} -> 
            NewInferences = case Inferences of
                #{InferenceType := PrevInferenceIds} -> Inferences#{InferenceType => [InferenceId | PrevInferenceIds]};
                _ -> Inferences#{InferenceType => [InferenceId]}
            end,
            process_events(State#state{inferences=NewInferences});

        {on, StimulationsCount, InferenceId} ->
            NewTotalONStimulations = TotalONStimulations + StimulationsCount,
            NewONStimulationsByInference = case ONStimulationsByInference of
                #{InferenceId := PrevStimulationsCount} -> ONStimulationsByInference#{InferenceId => PrevStimulationsCount + StimulationsCount};
                _ -> ONStimulationsByInference#{InferenceId => StimulationsCount}
            end,
            process_events(State#state{total_on_stimulations=NewTotalONStimulations, on_stimulations_by_inference=NewONStimulationsByInference});

        {vn, VNGName, VNStimulationsCount, InferenceId} ->
            NewTotalVNStimulations = case TotalVNStimulations of
                #{VNGName := VNGStimulationsCount} -> TotalVNStimulations#{VNGName => VNGStimulationsCount + VNStimulationsCount};
                _ -> TotalVNStimulations#{VNGName => VNStimulationsCount}
            end,

            NewVNStimulationsByInference = case VNStimulationsByInference of
                #{InferenceId := PrevStimulations} -> case PrevStimulations of
                    #{VNGName := PrevVNGStimulationsCount} -> VNStimulationsByInference#{InferenceId => PrevStimulations#{VNGName => PrevVNGStimulationsCount + VNStimulationsCount}};
                    _ -> VNStimulationsByInference#{InferenceId => PrevStimulations#{VNGName => VNStimulationsCount}}
                end;
                _ -> VNStimulationsByInference#{InferenceId => #{VNGName => VNStimulationsCount}}
            end,

            process_events(State#state{total_vn_stimulations=NewTotalVNStimulations, vn_stimulations_by_inference=NewVNStimulationsByInference});

        {print_report, Asker} ->
            OutputFile = "./stimulation_counts.out",

            % General stimulations count
            file:write_file(OutputFile, io_lib:fwrite("Stimulations counts:~nON stimulations: ~p~n", [TotalONStimulations]), [write]),
            maps:foreach(fun (VNGName, StimulationsCount) -> 
                file:write_file(
                    OutputFile,
                    io_lib:fwrite("VNG[~p] stimulations: ~p~n", [VNGName, StimulationsCount]),
                    [append]
                )
            end, TotalVNStimulations),

            % Stimulations by inference type
            file:write_file(OutputFile, io_lib:fwrite("~n~nStimulations by inference type:~n", []), [append]),
            maps:foreach(fun (InferenceType, InferenceIds) -> 
                
                file:write_file(
                    OutputFile,
                    io_lib:fwrite("~n~p:~n", [InferenceType]),
                    [append]
                ),
                
                StimulationCountsByNodeGroup = lists:foldl(
                    fun (InferenceId, Acc) -> 
                        AccWithVNGs = maps:fold(
                            fun (VNGName, VNStimulationsCount, InnerAcc) ->
                                case InnerAcc of
                                    #{VNGName := PrevVNGStimulationsCount} -> InnerAcc#{VNGName => PrevVNGStimulationsCount + VNStimulationsCount};
                                    _ -> InnerAcc#{VNGName => VNStimulationsCount}
                                end
                            end,
                            Acc,
                            maps:get(InferenceId, VNStimulationsByInference, #{})   % THis default value is shady - it should never be neccessary...
                        ),

                        case AccWithVNGs of
                            #{ong := ONStimulationsCount} -> AccWithVNGs#{ong => ONStimulationsCount + maps:get(InferenceId, ONStimulationsByInference, 0)};  % THis default value is shady - it should never be neccessary...
                            _ -> AccWithVNGs#{ong => maps:get(InferenceId, ONStimulationsByInference, 0)}  % THis default value is shady - it should never be neccessary...
                        end
                    end,
                    #{},
                    InferenceIds
                ),

                maps:foreach(fun (VNGName, VNStimulationsCount) -> 
                    file:write_file(
                        OutputFile,
                        io_lib:fwrite("   VNG[~p] stimulations: ~p~n", [VNGName, VNStimulationsCount]),
                        [append]
                    )
                end, StimulationCountsByNodeGroup)
            end, Inferences),

            Asker ! report_printed
    end.
