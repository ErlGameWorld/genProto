%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.8.0

-ifndef(mytest).
-define(mytest, true).

-define(mytest_gpb_version, "4.8.0").

-ifndef('TEST_PB_H').
-define('TEST_PB_H', true).
-record(test,
        {aa                     :: iodata()         % = 1
        }).
-endif.

-ifndef('PHONENUMBER_PB_H').
-define('PHONENUMBER_PB_H', true).
-record(phoneNumber,
        {number                 :: mytest:test(),   % = 1
         type                   :: integer()        % = 2, 32 bits
        }).
-endif.

-ifndef('PERSON_PB_H').
-define('PERSON_PB_H', true).
-record(person,
        {name                   :: iodata(),        % = 1
         integer                :: integer(),       % = 2, 32 bits
         email                  :: iodata() | undefined, % = 3
         phone = []             :: [mytest:phoneNumber()] | undefined % = 4
        }).
-endif.

-ifndef('ADDRESSBOOK_PB_H').
-define('ADDRESSBOOK_PB_H', true).
-record(addressBook,
        {person1 = []           :: [mytest:person()] | undefined, % = 1
         others = []            :: [mytest:person()] | undefined % = 2
        }).
-endif.

-ifndef('TINT32_PB_H').
-define('TINT32_PB_H', true).
-record(tint32,
        {int1                   :: integer(),       % = 1, 32 bits
         int2                   :: integer(),       % = 2, 32 bits
         int3                   :: integer(),       % = 3, 32 bits
         int4                   :: integer(),       % = 4, 32 bits
         int5                   :: integer(),       % = 5, 32 bits
         int6                   :: integer(),       % = 6, 32 bits
         int7                   :: integer(),       % = 7, 32 bits
         int8                   :: integer(),       % = 8, 32 bits
         int9                   :: integer(),       % = 9, 32 bits
         int10                  :: integer()        % = 10, 32 bits
        }).
-endif.

-endif.
