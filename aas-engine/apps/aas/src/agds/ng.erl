-module(ng).
% ng is an abbreviation for Node Group

-export([is_responsive/2, is_transitive/2]).



is_responsive(NodeGroupName, NodeGroupModes) -> 
    case maps:get(NodeGroupName, NodeGroupModes) of
        {responsive, _AmplificationFactor} -> true;
        _ -> false
    end.


is_transitive(NodeGroupName, NodeGroupModes) ->
    case maps:get(NodeGroupName, NodeGroupModes) of
        transitive -> true;
        _ -> false
    end.

