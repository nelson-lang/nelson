%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'red', 'blue', 'red'});
A = addcats(A, 'green');
asserts.istrue(iscategory(A, 'green'));
A = renamecats(A, 'green', 'yellow');
asserts.isfalse(iscategory(A, 'green'));
asserts.istrue(iscategory(A, 'yellow'));
A = reordercats(A, {'red', 'blue', 'yellow'});
asserts.isequal(categories(A), {'red'; 'blue'; 'yellow'});
A = removecats(A, 'yellow');
asserts.isfalse(iscategory(A, 'yellow'));
A = setcats(A, {'red'});
asserts.isequal(categories(A), {'red'});
%=============================================================================
B = categorical({'a', 'b', 'c'});
B = mergecats(B, {'b', 'c'}, 'bc');
asserts.isequal(categories(B), {'a'; 'bc'});
%=============================================================================
