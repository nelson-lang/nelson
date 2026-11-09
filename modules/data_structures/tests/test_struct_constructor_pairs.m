%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% struct(name, value, ...) builds one element when no value is a cell, one
% element per cell element otherwise, and checks every field name.
%=============================================================================
s = struct('a', 1, 'b', 'text', 'c_2', [1 2 3]);
assert_isequal(size(s), [1 1]);
assert_isequal(fieldnames(s), {'a'; 'b'; 'c_2'});
assert_isequal(s.a, 1);
assert_isequal(s.b, 'text');
assert_isequal(s.c_2, [1 2 3]);
s = struct('h', @sin, 'n', int8(4), 'e', []);
assert_isequal(s.n, int8(4));
assert_istrue(isempty(s.e));
assert_istrue(isa(s.h, 'function_handle'));
%=============================================================================
s = struct('a', {1, 2}, 'b', 'shared');
assert_isequal(size(s), [1 2]);
assert_isequal(s(2).a, 2);
assert_isequal(s(2).b, 'shared');
s = struct('a', {{1, 2}});
assert_isequal(s.a, {1, 2});
s = struct('a', "text");
assert_istrue(isstring(s.a));
assert_isequal(size(struct('a', {})), [0 0]);
%=============================================================================
assert_checkerror('struct(''1a'', 1)', 'requires a valid fieldname.');
assert_checkerror('struct(''a b'', 1)', 'requires a valid fieldname.');
assert_checkerror('struct(''_a'', 1)', 'requires a valid fieldname.');
assert_checkerror(['struct(''', char(233), ''', 1)'], 'requires a valid fieldname.');
assert_checkerror('struct(''a'', 1, ''b'')', 'requires pairs of field names and values.');
assert_checkerror('struct(1, 2)', 'requires pairs of field names and values.');
assert_checkerror('struct(''a'', {1, 2}, ''b'', {1, 2, 3})', ...
  'Dimensions of non-scalar entries must agree in structure construction.');
%=============================================================================
