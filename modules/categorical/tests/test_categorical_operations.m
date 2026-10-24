%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'b', 'a', 'b'});
asserts.isequal(A == 'b', [true false true]);
asserts.isequal(A ~= 'b', [false true false]);
asserts.isequal(countcats(A), [1 2]);
%=============================================================================
B = [categorical({'b'}), categorical({'a'})];
asserts.isequal(categories(B), {'b'; 'a'});
asserts.isequal(cellstr(B), {'b', 'a'});
%=============================================================================
C = categorical({'left'; 'right'}) .* categorical({'one'; 'two'});
asserts.isequal(categories(C), {'left one'; 'left two'; 'right one'; 'right two'});
asserts.isequal(cellstr(C), {'left one'; 'right two'});
%=============================================================================
[U, ia, ic] = unique(A);
asserts.isequal(cellstr(U), {'a'; 'b'});
%=============================================================================
