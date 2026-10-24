%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('getfield'), 2);
asserts.isequal(nargout('getfield'), 1);
%=============================================================================
S.alpha = 1;
S.beta = 'Nelson';
asserts.isequal(getfield(S, 'alpha'), 1);
asserts.isequal(getfield(S, 'beta'), 'Nelson');
%=============================================================================
S = struct('d', 4);
asserts.isequal(getfield(S, 'd'), 4);
%=============================================================================
% cells of subscripts between the field names
S = struct('a', [1 2 3; 4 5 6]);
asserts.isequal(getfield(S, 'a', {2}), 4);
asserts.isequal(getfield(S, 'a', {2, 3}), 6);
asserts.isequal(getfield(S, 'a', {':'}), [1; 4; 2; 5; 3; 6]);
asserts.isequal(getfield(S, 'a', {}), [1 2 3; 4 5 6]);
S = struct('a', {1, 2, 3});
asserts.isequal(getfield(S, {2}, 'a'), 2);
asserts.isequal(getfield(S, {1, 3}, 'a'), 3);
S = struct('a', struct('b', [7 8 9]));
asserts.isequal(getfield(S, 'a', 'b', {3}), 9);
%=============================================================================
