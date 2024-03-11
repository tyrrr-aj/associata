-module(agds_tests).
% -include_lib("eunit/include/eunit.hrl").


% % ****************************** BUILD AGDS TESTS ******************************

% build_test_() ->
%     {
%         "Tests training/data exposition of AGDS",
%         {
%             foreach,
%             fun create_sample_agds/0,
%             fun delete_agds/1,
%             [
%                 fun test_build_beer_agds/1
%             ]
%         }
%     }.

% test_build_beer_agds(AGDS) ->
%     ?_assertMatch(AGDS, train_sample_beer_agds(AGDS)).



% % ****************************** INFERENCE TESTS ******************************

% inference_test_() ->
%     {
%         "Tests inference operation on AGDS",
%         {
%             foreach,
%             fun sample_beer_agds/0,
%             fun delete_agds/1,
%             [
%                 fun test_inference_no_init_stimulation/1,
%                 fun test_inference_single_VN_stimulation_depth_1/1,
%                 fun test_inference_single_ON_stimulation_depth_1/1,
%                 fun test_inference_single_VN_stimulation_depth_2/1
%             ]
%         }
%     }.


% test_inference_no_init_stimulation(SampleAGDS) ->
%     ?_assertMatch(_, agds:infere(SampleAGDS, #{}, 3)).
    


% test_inference_single_VN_stimulation_depth_1(SampleAGDS) ->
%     ?_assertMatch(_, agds:infere(SampleAGDS, #{{vn, style, "Weizen"} => 1.0}, 1)).


% test_inference_single_ON_stimulation_depth_1(SampleAGDS) ->
%     ?_assertMatch(_, agds:infere(SampleAGDS, #{{on, 3} => 1.0}, 1)).


% test_inference_single_VN_stimulation_depth_2(SampleAGDS) ->
%     ?_assertMatch(_, agds:infere(SampleAGDS, #{{vn, plato, 12.0} => 1.0}, 2)).


% % ****************************** HELPERS ******************************

% sample_beer_agds() ->
%     AGDS = create_sample_agds(),
%     train_sample_beer_agds(AGDS).

% create_sample_agds() -> AGDS = agds:create().

% train_sample_beer_agds(AGDS) ->
%     agds:add_VNG(AGDS, plato, numerical),
%     agds:add_VNG(AGDS, ibu, numerical),
%     agds:add_VNG(AGDS, ebc, numerical),
%     agds:add_VNG(AGDS, style, categorical),

%     agds:add_observation(AGDS, #{plato => 12, ibu => 20, ebc => 40, style => "Weizen"}),
%     agds:add_observation(AGDS, #{plato => 16, ibu => 40, ebc => 100, style => "Bock"}),
%     agds:add_observation(AGDS, #{plato => 22, ibu => 50, ebc => 400, style => "Baltic Porter"}),
%     agds:add_observation(AGDS, #{plato => 20, ibu => 50, ebc => 500, style => "Stout"}),
%     agds:add_observation(AGDS, #{plato => 15, ibu => 70, ebc => 70, style => "IPA"}),
%     agds:add_observation(AGDS, #{plato => 14, ibu => 60, ebc => 60, style => "APA"}),
%     agds:add_observation(AGDS, #{plato => 30, ibu => 80, ebc => 800, style => "Imperial Stout"}),
%     agds:add_observation(AGDS, #{plato => 30, ibu => 70, ebc => 700, style => "Imperial Baltic Porter"}),

%     agds:end_experiment(AGDS),

%     AGDS.

% delete_agds(AGDS) -> agds:delete(AGDS).
