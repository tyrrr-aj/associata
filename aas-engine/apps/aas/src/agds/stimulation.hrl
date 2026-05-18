-record(stim_spec, {
    stimulation_id, 
    experiment_step, 
    stimulation_name, 
    should_write_to_log, 
    stimulation_kind, 
    node_group_modes, 
    min_passed_stimulus, 
    poisoning_params,
    vn_to_vn_weight_mode,  % constant | classical_multiplicative | classical_subtractive
    vn_to_on_weight_mode    % constant | one_over_n_on | rate_of_occurance
}).