%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'b', 'a', '', 'b'});
asserts.isequal(class(A), 'categorical');
asserts.isequal(size(A), [1 4]);
asserts.isequal(categories(A), {'a'; 'b'});
asserts.isequal(isundefined(A), [false false true false]);
asserts.isequal(cellstr(A), {'b', 'a', '<undefined>', 'b'});
%=============================================================================
B = categorical([1 2 3 4], [1 2 3], {'low', 'mid', 'high'});
asserts.isequal(categories(B), {'low'; 'mid'; 'high'});
asserts.isequal(isundefined(B), [false false false true]);
%=============================================================================
C = categorical({'x', 'y'}, {'x', 'y'}, {'same', 'same'});
asserts.isequal(categories(C), {'same'});
asserts.isequal(double(C), [1 1]);
%=============================================================================
D = categorical({'red'; 'blue'; 'red'; 'green'});
asserts.isequal(size(D), [4 1]);
asserts.isequal(categories(D), {'blue'; 'green'; 'red'});
asserts.isequal(double(D), [3; 1; 3; 2]);
%=============================================================================
