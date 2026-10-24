%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'red', 'blue', 'red'}, {'red', 'blue'}, {'red', 'blue'});
A = addcats(A, {'green', 'blue'});
asserts.isequal(categories(A), {'red'; 'blue'; 'green'});
A = addcats(A, 'amber', 'Before', 'blue');
asserts.isequal(categories(A), {'red'; 'amber'; 'blue'; 'green'});
asserts.isequal(cellstr(A), {'red', 'blue', 'red'});
A = addcats(A, 'violet', 'After', 'green');
asserts.isequal(categories(A), {'red'; 'amber'; 'blue'; 'green'; 'violet'});
%=============================================================================
B = removecats(A);
asserts.isequal(categories(B), {'red'; 'blue'});
C = removecats(A, {'amber', 'notPresent'});
asserts.isequal(categories(C), {'red'; 'blue'; 'green'; 'violet'});
%=============================================================================
D = renamecats(C, {'scarlet', 'azure', 'lime', 'purple'});
asserts.isequal(categories(D), {'scarlet'; 'azure'; 'lime'; 'purple'});
D = renamecats(D, {'scarlet', 'purple'}, {'red', 'violet'});
asserts.isequal(categories(D), {'red'; 'azure'; 'lime'; 'violet'});
%=============================================================================
E = reordercats(D);
asserts.isequal(categories(E), {'azure'; 'lime'; 'red'; 'violet'});
F = reordercats(E, [3 1 2 4]);
asserts.isequal(categories(F), {'red'; 'azure'; 'lime'; 'violet'});
G = setcats(F, {'red', 'violet'});
asserts.isequal(categories(G), {'red'; 'violet'});
asserts.isequal(cellstr(G), {'red', '<undefined>', 'red'});
%=============================================================================
H = categorical({'a', 'b', 'c', 'b'});
H = mergecats(H, {'b', 'c'});
asserts.isequal(categories(H), {'a'; 'b'});
asserts.isequal(cellstr(H), {'a', 'b', 'b', 'b'});
H = mergecats(H, {'a', 'b'}, 'all');
asserts.isequal(categories(H), {'all'});
asserts.isequal(cellstr(H), {'all', 'all', 'all', 'all'});
%=============================================================================
P = categorical({'red', 'rose', 'blue', 'brown'});
asserts.istrue(iscategory(P, "r" + wildcardPattern));
P = removecats(P, "br" + wildcardPattern);
asserts.isequal(categories(P), {'blue'; 'red'; 'rose'});
asserts.isequal(cellstr(P), {'red', 'rose', 'blue', '<undefined>'});
P = mergecats(P, "r" + wildcardPattern, 'warm');
asserts.isequal(categories(P), {'blue'; 'warm'});
asserts.isequal(cellstr(P), {'warm', 'warm', 'blue', '<undefined>'});
%=============================================================================
asserts.checkerror('addcats(categorical({''a''}, ''Ordinal'', true), ''b'')', ...
  _('Before or After must be specified for ordinal categorical arrays.'));
asserts.checkerror('addcats(A, ''x'', ''Near'', ''red'')', sprintf(_('Invalid %s option.'), 'addcats'));
asserts.checkerror('addcats(A, ''x'', ''Before'', ''missing'')', _('Category not found.'));
asserts.checkerror('addcats(A, ''x'', ''Before'', ''missing'')', _('Category not found.'), 'Nelson:categorical:categoryNotFound');
asserts.checkerror('renamecats(A, {''red''}, {''x'', ''y''})', ...
  _('Old and new category names must have the same length.'));
asserts.checkerror('renamecats(A, {''red''}, {''x'', ''y''})', ...
  _('Old and new category names must have the same length.'), 'Nelson:renamecats:lengthMismatch');
asserts.checkerror('renamecats(A, {''red''}, {''blue''})', _('Category names must be unique.'));
asserts.checkerror('renamecats(A, {''red''}, {''blue''})', _('Category names must be unique.'), 'Nelson:categorical:categoryNamesMustBeUnique');
asserts.checkerror('reordercats(A, {''red'', ''blue''})', ...
  _('New category order must contain all categories.'));
asserts.checkerror('setcats(A, {''red'', ''red''})', _('Category names must be unique.'));
%=============================================================================
