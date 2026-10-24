%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% RowNames assigned as a row are stored as a column
%=============================================================================
T = table([1; 2]);
T.Properties.RowNames = {'a', 'b'};
assert_isequal(T.Properties.RowNames, {'a'; 'b'});
T.Properties.RowNames = {'c', 'd'};
assert_isequal(T.Properties.RowNames, {'c'; 'd'});
T.Properties.RowNames = ["e", "f"];
assert_isequal(T.Properties.RowNames, {'e'; 'f'});
P = T.Properties;
P.RowNames = {'g', 'h'};
T.Properties = P;
assert_isequal(T.Properties.RowNames, {'g'; 'h'});
assert_isequal(T{'h', 1}, 2);
%=============================================================================
T = table([1; 2], 'RowNames', {'a', 'b'});
assert_isequal(T.Properties.RowNames, {'a'; 'b'});
%=============================================================================
% same count: the names are still checked
T = table([1; 2], 'RowNames', {'a'; 'b'});
msg = _('The RowNames property must be a string array or a cell array, with each name containing one or more characters.');
assert_checkerror('T.Properties.RowNames = {''x'', 5};', msg, 'Nelson:table:invalidRowNames');
T.Properties.RowNames = {};
assert_isequal(size(T.Properties.RowNames), [0 0]);
%=============================================================================
