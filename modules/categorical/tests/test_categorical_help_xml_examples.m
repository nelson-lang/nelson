%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'red', 'blue'}); B = addcats(A, 'green'); C = categories(B);
asserts.isequal(C, {'blue'; 'red'; 'green'});
%=============================================================================
C = categorical({'red', 'blue', 'red'}); C = categories(C);
asserts.isequal(C, {'blue'; 'red'});
%=============================================================================
A = categorical({'small', 'large'}); B = categorical({'red', 'blue'});
T = combinations(A, B);
asserts.isequal(size(T), [4 2]);
%=============================================================================
A = categorical({'red', 'blue', 'red', ''}); counts = countcats(A);
asserts.isequal(counts, [1 2]);
%=============================================================================
A = categorical({'red', 'blue', 'red'}); counts = histcounts(A);
asserts.isequal(counts, [1 2]);
%=============================================================================
A = categorical({'red', 'blue'}); tf = iscategorical(A);
asserts.istrue(tf);
%=============================================================================
A = categorical({'red', 'blue'}); tf = iscategory(A, {'red', 'green'});
asserts.isequal(tf, [true false]);
%=============================================================================
A = categorical({'low', 'high'}, {'low', 'high'}, 'Ordinal', true);
tf = isordinal(A);
asserts.istrue(tf);
%=============================================================================
A = categorical({'low', 'high'}, {'low', 'high'}, 'Protected', true);
tf = isprotected(A);
asserts.istrue(tf);
%=============================================================================
A = categorical({'red', '', 'blue'}); tf = isundefined(A);
asserts.isequal(tf, [false true false]);
%=============================================================================
A = categorical({'red', 'blue', 'green'});
B = mergecats(A, {'blue', 'green'}, 'other');
C = categories(B);
asserts.isequal(C, {'other'; 'red'});
%=============================================================================
A = categorical({'red', 'blue'}, {'red', 'blue', 'green'});
B = removecats(A, 'green');
C = categories(B);
asserts.isequal(C, {'red'; 'blue'});
%=============================================================================
A = categorical({'red', 'blue'});
B = renamecats(A, 'red', 'rouge');
C = categories(B);
asserts.isequal(C, {'blue'; 'rouge'});
%=============================================================================
A = categorical({'red', 'blue'}, {'red', 'blue'});
B = reordercats(A, {'blue', 'red'});
C = categories(B);
asserts.isequal(C, {'blue'; 'red'});
%=============================================================================
A = categorical({'red', 'blue', 'green'});
B = setcats(A, {'red', 'blue'});
tf = isundefined(B);
asserts.isequal(tf, [false false true]);
%=============================================================================
