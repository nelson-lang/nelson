%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('setfield'), -2);
asserts.isequal(nargout('setfield'), 1);
%=============================================================================
R = {};
R = setfield(R, 'toto', 3);
REF = struct('toto', 3);
asserts.isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
R = setfield(R, 'titi', 4);
REF = struct('toto', 3, 'titi', 4);
asserts.isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
R = setfield(R, "titi", 4);
REF = struct('toto', 3, 'titi', 4);
asserts.isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
asserts.checkerror('R = setfield(R, 19, 4);', _('Input should be a string or character array.'));
%=============================================================================
S = struct('a', 1);
S = setfield(S, 'd', 4);
asserts.isequal(getfield(S, 'd'), 4);
asserts.checkerror('setfield(struct(), 19, 4)', _('Input should be a string or character array.'));
%=============================================================================
% cells of subscripts between the field names
S = struct('a', [1 2 3]);
R = setfield(S, 'a', {2}, 9);
asserts.isequal(R.a, [1 9 3]);
R = setfield(S, 'a', {5}, 9);
asserts.isequal(R.a, [1 2 3 0 9]);
S = struct('a', [1 2 3; 4 5 6]);
R = setfield(S, 'a', {2, 3}, 9);
asserts.isequal(R.a, [1 2 3; 4 5 9]);
S = struct('a', {1, 2, 3});
R = setfield(S, {2}, 'a', 9);
asserts.isequal([R.a], [1 9 3]);
R = setfield(struct('a', {1, 2}), {4}, 'a', 9);
asserts.isequal(size(R), [1 4]);
asserts.isequal(R(4).a, 9);
asserts.isequal(R(3).a, []);
S = struct('a', struct('b', [7 8 9]));
R = setfield(S, 'a', 'b', {3}, 0);
asserts.isequal(R.a.b, [7 8 0]);
%=============================================================================
