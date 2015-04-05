-ifndef(_hibari_types_included).
-define(_hibari_types_included, yeah).

-define(hibari_KeepOrReplace_Keep, 1).
-define(hibari_KeepOrReplace_Replace, 2).

%% struct property

-record(property, {key :: string() | binary(),
                   value :: string() | binary()}).

%% struct writeOptions

-record(writeOptions, {exp_time :: integer(),
                       test_set :: integer(),
                       exp_time_directive :: integer(),
                       attrib_directive :: integer(),
                       value_in_ram :: boolean()}).

%% struct deleteOptions

-record(deleteOptions, {test_set :: integer(),
                        must_exist :: boolean(),
                        must_not_exist :: boolean()}).

%% struct readOptions

-record(readOptions, {test_set :: integer(),
                      is_witness :: boolean(),
                      get_all_attribs :: boolean(),
                      must_exist :: boolean(),
                      must_not_exist :: boolean()}).

%% struct doTransaction

-record(doTransaction, {}).

%% struct doAdd

-record(doAdd, {key :: string() | binary(),
                value :: string() | binary(),
                properties :: list(),
                write_options = #writeOptions{} :: #writeOptions{}}).

%% struct doReplace

-record(doReplace, {key :: string() | binary(),
                    value :: string() | binary(),
                    properties :: list(),
                    write_options = #writeOptions{} :: #writeOptions{}}).

%% struct doSet

-record(doSet, {key :: string() | binary(),
                value :: string() | binary(),
                properties :: list(),
                write_options = #writeOptions{} :: #writeOptions{}}).

%% struct doRename

-record(doRename, {key :: string() | binary(),
                   new_key :: string() | binary(),
                   properties :: list(),
                   write_options :: #writeOptions{}}).

%% struct doGet

-record(doGet, {key :: string() | binary(),
                read_options :: #readOptions{}}).

%% struct doGetMany

-record(doGetMany, {key :: string() | binary(),
                    max_keys :: integer(),
                    read_options :: #readOptions{}}).

%% struct doDelete

-record(doDelete, {key :: string() | binary(),
                   write_options :: #writeOptions{}}).

%% struct op

-record(op, {txn :: #doTransaction{},
             add_kv :: #doAdd{},
             replace_kv :: #doReplace{},
             set_kv :: #doSet{},
             rename_kv :: #doRename{},
             get_kv :: #doGet{},
             get_many :: #doGetMany{},
             delete_kv :: #doDelete{}}).

%% struct doOptions

-record(doOptions, {}).

%% struct getResponse

-record(getResponse, {timestamp :: integer(),
                      value :: string() | binary(),
                      exp_time :: integer(),
                      proplist :: list()}).

%% struct getManyResponse

-record(getManyResponse, {records = [] :: list(),
                          is_truncated :: boolean()}).

%% struct doFailure

-record(doFailure, {}).

%% struct doResponse

-record(doResponse, {}).

%% struct serviceNotAvailableException

-record(serviceNotAvailableException, {}).

%% struct notImplementedException

-record(notImplementedException, {}).

%% struct timedOutException

-record(timedOutException, {}).

%% struct tSErrorException

-record(tSErrorException, {}).

%% struct keyExistisException

-record(keyExistisException, {}).

%% struct keyNotExistsException

-record(keyNotExistsException, {}).

%% struct invalidOptionPresentException

-record(invalidOptionPresentException, {option :: string() | binary(),
                                        value :: string() | binary()}).

%% struct transactionFailureException

-record(transactionFailureException, {do_op_index :: integer(),
                                      do_failure = #doFailure{} :: #doFailure{}}).

-endif.
