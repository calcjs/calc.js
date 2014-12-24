%% form_id, order, field_name, repeating, formula, always_exec, label, active
-type calc_data() :: {atom(), integer(), string(), boolean(), string(), boolean(), boolean(), boolean()}.
%% form_id, data_retrieval_api, post_api, field_info_api
-type dependent_form() :: {atom(), string(), string(), string()}.

% record for a calcjs form
-record(calcjs_form, {id :: string(),
                      active :: boolean(),
                      description :: string(),
                      dependent_forms :: [dependent_form()],
                      calc_data :: [calc_data()],
                      data_retrieval_api :: string(),
                      post_api :: string(),
                      field_info_api :: string(),
                      creation_date :: erlang:timestamp(),
                      last_updated_date :: erlang:timestamp()}).


% record for a calcjs configuration
-record(calcjs_conf, {id :: string(),
                      input_attribute_type :: string(),
                      last_updated_date :: erlang:timestamp()}).
