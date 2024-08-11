-module(pyrlang).


-export([send_client/2, send_vis/2]).


send_client(ChannelId, Msg) -> {ChannelId, associata@Beast} ! Msg.

send_vis(ChannelId, Msg) -> {ChannelId, aas_vis@Beast} ! Msg.
