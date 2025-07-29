%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('b', 3, 'd', 7, 'a', 4);
R = renameStructField(s, 'c', 'd');
assert_isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('a', 3, 'b', 7, 'c', 4);
R = renameStructField(s, {'a','b','c'});
assert_isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('c', 3, 'a', 7, 'b', 4);
R = renameStructField(s, {'c','a','b'});
assert_isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('c', 3, 'a', 7, 'b', 4);
R = renameStructField(s, ["c","a","b"]);
assert_isequal(R, REF);
%=============================================================================
s = struct('bla', 3, 'c', 7, 'bli', 4);
REF = struct('blabla', 3, 'c', 7, 'bli', 4);
R = renameStructField(s, 'bla', 'blabla');
assert_isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
msg = _('Number of field names must match number of desired names.');
assert_checkerror('R = renameStructField(s, ["b"]);', msg);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
R = renameStructField(s, ["c","a","b"], ["c","a","b"]);
assert_isequal(s, R);
%=============================================================================
