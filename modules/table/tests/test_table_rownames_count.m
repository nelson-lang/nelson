%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% RowNames assigned through Properties must name every row
%=============================================================================
msg = _('The RowNames property must contain one name for each row in the table.');
id = 'Nelson:table:incorrectNumberOfRowNames';
T = table([1; 2], [3; 4]);
assert_checkerror('T.Properties.RowNames = {''a'', ''b'', ''c''};', msg, id);
assert_checkerror('T.Properties.RowNames = {''a''};', msg, id);
assert_checkerror('T.Properties.RowNames = "a";', msg, id);
assert_checkerror('T.Properties.RowNames = ["a"; "b"; "c"];', msg, id);
assert_checkerror('T.Properties = setfield(T.Properties, ''RowNames'', {''a''});', msg, id);
assert_isequal(T.Properties.RowNames, {});
% invalid names are reported before their count
msgInvalid = _('The RowNames property must be a string array or a cell array, with each name containing one or more characters.');
assert_checkerror('T.Properties.RowNames = ''a'';', msgInvalid, 'Nelson:table:invalidRowNames');
assert_checkerror('T.Properties.RowNames = {''a'', '''', ''c''};', msgInvalid, 'Nelson:table:invalidRowNames');
assert_checkerror('T.Properties.RowNames = {1, 2, 3};', msgInvalid, 'Nelson:table:invalidRowNames');
assert_checkerror('T.Properties.RowNames = {''a'', ''a'', ''c''};', msg, id);
%=============================================================================
T.Properties.RowNames = {'a', 'b'};
assert_isequal(T.Properties.RowNames(:), {'a'; 'b'});
assert_checkerror('T.Properties.RowNames = {''a'', ''b'', ''c''};', msg, id);
T.Properties.RowNames = ["c"; "d"];
assert_isequal(T.Properties.RowNames(:), {'c'; 'd'});
T.Properties.RowNames = {};
assert_isequal(T.Properties.RowNames, {});
%=============================================================================
% zero-width table: n-by-0
T = table([1; 2]);
T(:, 1) = [];
assert_isequal(size(T), [2 0]);
assert_checkerror('T.Properties.RowNames = {''a'', ''b'', ''c''};', msg, id);
assert_checkerror('T.Properties.RowNames = {''a''};', msg, id);
T.Properties.RowNames = {'a', 'b'};
assert_isequal(size(T), [2 0]);
assert_isequal(T.Properties.RowNames(:), {'a'; 'b'});
%=============================================================================
% RowNames given with the variables are still accepted
T = table([1; 2], 'RowNames', {'a', 'b'});
assert_isequal(T.Properties.RowNames(:), {'a'; 'b'});
T = table('RowNames', {'a', 'b', 'c'});
assert_isequal(size(T), [3 0]);
T = table([1; 2; 3], 'RowNames', {'a', 'b', 'c'});
R = T(2:3, :);
assert_isequal(R.Properties.RowNames, {'b'; 'c'});
%=============================================================================
