%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the table set operations are called when any input is a table
%=============================================================================
A = table([1; 2]);
msg = _('First input must be a table.');
id = 'Nelson:table:invalidType';
asserts.checkerror('ismember(5, A)', msg, id);
asserts.checkerror('intersect(5, A)', msg, id);
asserts.checkerror('setdiff(5, A)', msg, id);
asserts.checkerror('union(5, A)', msg, id);
asserts.checkerror('setxor(5, A)', msg, id);
asserts.checkerror('union({''a''}, A)', msg, id);
asserts.checkerror('intersect("a", A, ''stable'')', msg, id);
%=============================================================================
msg = _('Second input must be a table.');
asserts.checkerror('union(A, 5)', msg, id);
asserts.checkerror('setxor(A, {''a''})', msg, id);
%=============================================================================
% without a table the numeric set functions are unchanged
asserts.isequal(intersect(5, [5, 6]), 5);
asserts.isequal(union(5, [6, 5]), [5, 6]);
asserts.isequal(ismember(5, [1, 5]), true);
asserts.isequal(setdiff({'a', 'b'}, {'b'}), {'a'});
%=============================================================================
