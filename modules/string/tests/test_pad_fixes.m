%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the pad character may follow the width directly, the side is any case
asserts.isequal(pad('abc', 5, '*'), 'abc**');
asserts.isequal(pad('abc', 5, 'left', '*'), '**abc');
asserts.isequal(pad('abc', 5, 'LEFT'), '  abc');
asserts.isequal(pad('abc', 5, 'l'), 'abcll');
asserts.isequal(pad('abc', 5, "x"), 'abcxx');
msg = 'Side must be ''left'', ''right'' or ''both'' and the pad character a single character.';
asserts.checkerror('R = pad("abc", 5, "");', msg, 'Nelson:string:pad:invalidArgument');
asserts.checkerror('R = pad(''abc'', 5, ''ab'');', msg, 'Nelson:string:pad:invalidArgument');
asserts.checkerror('R = pad(''abc'', 5, ''left'', ''both'');', msg, 'Nelson:string:pad:invalidArgument');
asserts.checkerror('R = pad(''abc'', 5, ''x'', ''right'');', msg, 'Nelson:string:pad:invalidArgument');
asserts.checkerror('R = pad(''abc'', ''x'', ''left'');', msg, 'Nelson:string:pad:invalidArgument');
%=============================================================================
% a negative, infinite or missing width used to crash
asserts.checkerror('R = pad(''abc'', -1);', 'Width must be a non-negative integer scalar.', 'Nelson:string:pad:invalidWidth');
asserts.checkerror('R = pad(''abc'', Inf);', 'Width must be a non-negative integer scalar.', 'Nelson:string:pad:invalidWidth');
R = pad(["a", missing]);
asserts.isequal(R(1), "a");
asserts.istrue(ismissing(R(2)));
%=============================================================================
