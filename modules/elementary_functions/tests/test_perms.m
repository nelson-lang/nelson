%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('perms'), 1);
asserts.isequal(nargout('perms'), 1);
%=============================================================================
asserts.isequal(perms([1 2 3]), [3 2 1; 3 1 2; 2 3 1; 2 1 3; 1 3 2; 1 2 3]);
asserts.isequal(perms([9 8]), [8 9; 9 8]);
asserts.isequal(perms(1), 1);
asserts.isequal(perms([2 4 6]), [6 4 2; 6 2 4; 4 6 2; 4 2 6; 2 6 4; 2 4 6]);
asserts.isequal(size(perms(1:5)), [120 5]);
% column input behaves like row input
asserts.isequal(perms([1; 2; 3]), perms([1 2 3]));
% last row is the original order
P = perms([10 20 30 40]);
asserts.isequal(P(end, :), [10 20 30 40]);
asserts.isequal(size(P), [24 4]);
% works with char
asserts.isequal(perms('ab'), ['ba'; 'ab']);
%=============================================================================
