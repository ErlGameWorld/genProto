-module(protoField).

-compile([export_all, nowarn_export_all]).

-define(SimpleList, [
   , "int8"
   , "uint8"
   , "int16"
   , "uint16"
   , "int32"
   , "uint32"
   , "int64"
   , "uint64"
   , "float"
   , "double"
   ]).

-define(TypeList, [
   "bool"
   , "int8"
   , "uint8"
   , "int16"
   , "uint16"
   , "int32"
   , "uint32"
   , "int64"
   , "uint64"
   , "integer"
   , "number"
   , "float"
   , "double"
   , "string"
]).

-define(TypeValue, [
   {"bool", "false", "boolean()"}
   , {"int8", "0", "int8()"}
   , {"uint8", "0", "uint8()"}
   , {"int16", "0", "int16()"}
   , {"uint16", "0", "uint16()"}
   , {"int32", "0", "int32()"}
   , {"uint32", "0", "uint32()"}
   , {"int64", "0", "int64()"}
   , {"uint64", "0", "uint64()"}
   , {"integer", "0", "integer()"}
   , {"number", "0", "number()"}
   , {"float", "0.0", "float()"}
   , {"double", "0.0", "double()"}
   , {"string", "\"\"", "string()"}
]).

builtRecStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, DefValueStr, DefTypeStr} ->
         NameStr ++ " = " ++ DefValueStr ++ " :: " ++ DefTypeStr ++ "\n";
      _ ->
         case TypeStr of
            "list[" ++ LeftStr ->
               [SubTypeStr | _] = re:split(LeftStr, "\\]", [{return, list}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, _DefSubValueStr, DefSubTypeStr} ->
                     NameStr ++ " = [] " ++ " :: [" ++ DefSubTypeStr ++ "]\n";
                  _ ->
                     NameStr ++ " = [] " ++ " :: [#" ++ SubTypeStr ++ "{}]\n"
               end;
            _ ->
               NameStr ++ " = undefined " ++ " :: #" ++ TypeStr ++ "{}\n"
         end
   end.

builtPackStr(TypeStr) ->
   case lists:member(TypeStr, ?TypeList) of
      true ->
         "?" ++ TypeStr ++ "(";
      _ ->
         case TypeStr of
            "list[" ++ LeftStr ->
               [SubTypeStr | _] = re:split(LeftStr, "\\]", [{return, list}]),
               SubStr =
                  case lists:member(SubTypeStr, ?TypeList) of
                     true ->
                        SubTypeStr;
                     _ ->
                        "record"
                  end,
               "?list_" ++ SubStr ++ "(";
            _ ->
               "?record("
         end
   end.

isBaseType(TypeStr) ->
   lists:member(TypeStr, ?TypeList).






