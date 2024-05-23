%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = struct('b', 2, 'c', 3, 'a', 1);
s2 = orderfields(s);
SREF = struct('a', 1, 'b', 2, 'c', 3);
assert_isequal(s2, SREF);
%=============================================================================
s = struct('b', 2, 'c', 3, 'a', 1);
[s2, p] = orderfields(s);
SREF = struct('a', 1, 'b', 2, 'c', 3);
PREF = [3; 1; 2];
assert_isequal(s2, SREF);
assert_isequal(p, PREF);
%=============================================================================
s = struct('b', 2, 'c', 3, 'a', 1);
[s2, p2] = orderfields(s, {'b','a','c'});
SREF = struct('b', 2, 'a', 1, 'c', 3);
PREF = [1; 3; 2];
assert_isequal(s2, SREF);
assert_isequal(p2, PREF);
%=============================================================================
[s2, p2] = orderfields(s, ["b", "a", "c"]);
SREF = struct('b', 2, 'a', 1, 'c', 3);
PREF = [1; 3; 2];
assert_isequal(s2, SREF);
assert_isequal(p2, PREF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
P = [3 1 2];
s2 = orderfields(s, P);
SREF = struct('a', 4, 'b', 3, 'c', 7);
assert_isequal(s2, SREF);
%=============================================================================
