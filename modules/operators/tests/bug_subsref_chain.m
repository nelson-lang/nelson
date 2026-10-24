%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% subsref applied every indexing step to the original value instead of
% chaining on the previous result
%=============================================================================
s.a = [1 2 3];
S = struct('type', {'.', '()'}, 'subs', {'a', {2}});
asserts.isequal(subsref(s, S), 2);
%=============================================================================
t.b = {7, 8};
S2 = struct('type', {'.', '{}'}, 'subs', {'b', {1}});
asserts.isequal(subsref(t, S2), 7);
%=============================================================================
% single step still works
c = {10, 20; 30, 40};
S3 = struct('type', {'{}'}, 'subs', {{2, 2}});
asserts.isequal(subsref(c, S3), 40);
S4 = struct('type', {'()'}, 'subs', {{1, 2}});
asserts.isequal(subsref([1 2; 3 4], S4), 2);
%=============================================================================
% three levels
u.v.w = [10 20 30];
S5 = struct('type', {'.', '.', '()'}, 'subs', {'v', 'w', {3}});
asserts.isequal(subsref(u, S5), 30);
%=============================================================================
