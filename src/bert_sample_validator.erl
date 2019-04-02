-module(bert_sample_validator).
-include("/Users/maxim/depot/o/o7/bpe/deps/bert/include/bert.hrl").
-include("/Users/maxim/depot/o/o7/bpe/deps/bert/include/sample.hrl").
-compile(export_all).
-define(COND_FUN(Cond), fun(Rec) when Cond -> true; (_) -> false end).

validate(Obj) -> validate(Obj, []).
validate(_, [{[_|_] , _R}|_] = Acc) -> {error, Acc};
validate([], _) -> ok;
validate(Objs, [{[] , R}|T]) -> validate(Objs, [R|T]);
validate([{CondFun, _, []}|T], Acc) when is_function(CondFun) -> validate(T, Acc);
validate([{CondFun, Field, [Obj|TObjs]}|T], Acc) when is_function(CondFun) ->
  case CondFun(Obj) of
    true -> validate([{CondFun, Field, TObjs}|T], Acc);
    false -> {error, [Field, Obj|Acc]} end;
validate([{CondFun, Field, Obj}|T], Acc) when is_function(CondFun) ->
  case CondFun(Obj) of true -> validate(T, Acc); false -> {error, [Field, Obj|Acc]} end;
validate([{_Field, []}|T], Acc) -> validate(T, Acc);
validate([{Field, [Obj|TObjs]}|T], Acc) ->
  case validate(Obj, [Field|Acc]) of
    ok -> validate([{Field, TObjs}|T], Acc);
    Err -> Err end;

validate(D = #'muc'{name = Name}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{name,_} when (is_binary(Name) orelse Name==[]) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'muc'), tl(tuple_to_list(D)))]),
	CondFuns = [],
	Fields = [],
	FieldNames = [],
	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;
validate(D = #'p2p'{from = From, to = To}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{from,_} when (is_binary(From) orelse From==[]) -> []; 
	{to,_} when (is_binary(To) orelse To==[]) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'p2p'), tl(tuple_to_list(D)))]),
	CondFuns = [],
	Fields = [],
	FieldNames = [],
	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;
validate(D = #'Feature'{id = Id, key = Key, value = Value, group = Group}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{id,_} when (is_binary(Id) orelse Id==[]) -> []; 
	{key,_} when (is_binary(Key) orelse Key==[]) -> []; 
	{value,_} when (is_binary(Value) orelse Value==[]) -> []; 
	{group,_} when (is_binary(Group) orelse Group==[]) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'Feature'), tl(tuple_to_list(D)))]),
	CondFuns = [],
	Fields = [],
	FieldNames = [],
	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;
validate(D = #'Desc'{id = Id, mime = Mime, payload = Payload, parentid = Parentid, data = Data}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{id,_} when is_binary(Id) -> []; 
	{mime,_} when is_binary(Mime) -> []; 
	{payload,_} when is_binary(Payload) -> []; 
	{parentid,_} when is_binary(Parentid) -> []; 
	{data,_} when is_list(Data) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'Desc'), tl(tuple_to_list(D)))]),
	CondFuns = [?COND_FUN(is_record(Rec, 'Feature'))],
	Fields = [Data],
	FieldNames = [data],	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;
validate(D = #'AuthError'{codes = Codes, data = Data}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{codes,_} when is_list(Codes) -> []; 
	{data,_} when (is_record(Data,'Auth') orelse Data==[]) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'AuthError'), tl(tuple_to_list(D)))]),
	CondFuns = [],
	Fields = [],
	FieldNames = [],	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;
validate(D = #'Member'{id = Id, container = Container, feed_id = Feed_id, prev = Prev, next = Next, feeds = Feeds, phone_id = Phone_id, avatar = Avatar, names = Names, surnames = Surnames, alias = Alias, reader = Reader, update = Update, settings = Settings, services = Services, presence = Presence, member_status = Member_status}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{id,_} when (is_integer(Id) orelse Id==[]) -> []; 
	{feed_id,_} when (is_record(Feed_id,'p2p') orelse is_record(Feed_id,'muc')) -> []; 
	{prev,_} when (is_integer(Prev) orelse Prev==[]) -> []; 
	{next,_} when (is_integer(Next) orelse Next==[]) -> []; 
	{feeds,_} when is_list(Feeds) -> []; 
	{phone_id,_} when (is_binary(Phone_id) orelse Phone_id==[]) -> []; 
	{avatar,_} when (is_binary(Avatar) orelse Avatar==[]) -> []; 
	{names,_} when (is_binary(Names) orelse Names==[]) -> []; 
	{surnames,_} when (is_binary(Surnames) orelse Surnames==[]) -> []; 
	{alias,_} when (is_binary(Alias) orelse Alias==[]) -> []; 
	{reader,_} when (is_integer(Reader) orelse Reader==[]) -> []; 
	{update,_} when (is_integer(Update) orelse Update==[]) -> []; 
	{settings,_} when is_list(Settings) -> []; 
	{services,_} when is_list(Services) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'Member'), tl(tuple_to_list(D)))]),
	CondFuns = [?COND_FUN(is_record(Rec, 'Feature')),?COND_FUN(is_record(Rec, 'Service'))],
	Fields = [Settings,Services],
	FieldNames = [settings,services],	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;
validate(D = #'ExtendedStar'{star = Star, from = From}, Acc) -> 
	ErrFields = lists:flatten(
		  [case {Field, F} of
	{star,_} when is_record(Star,'ExtendedStar') -> []; 
	{from,_} when (is_record(From,'Room') orelse is_record(From,'Contact')) -> [];
	_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, 'ExtendedStar'), tl(tuple_to_list(D)))]),
	CondFuns = [],
	Fields = [],
	FieldNames = [],	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end.