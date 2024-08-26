-module(stimulation).

-export([respond_to_stimulation/2, send_stimulation_finished/2, send_stimulation_finished/3]).



% called only from nodes in responsive mode
respond_to_stimulation(Node, Stimulus) -> 
    Node ! {stimulation_response, Stimulus}.


send_stimulation_finished(Stimulator, Depth, ConfirmationCount) ->
    Stimulator ! {stimulation_finished, self(), Depth, ConfirmationCount}.


send_stimulation_finished(Stimulator, Depth) ->
    send_stimulation_finished(Stimulator, Depth, 1).
