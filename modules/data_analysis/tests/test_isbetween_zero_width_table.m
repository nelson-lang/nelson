%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A table without variables keeps its number of rows in C++ consumers.
T = table('Size', [2 0], 'VariableTypes', {});
R = isbetween(T, 0, 1);
asserts.isequal(class(R), 'logical');
asserts.isequal(size(R), [2 0]);
%=============================================================================
R = isbetween(T, 0, 1, 'OutputFormat', 'tabular');
asserts.istrue(istable(R));
asserts.isequal(size(R), [2 0]);
%=============================================================================
T.Properties.RowNames = {'r1', 'r2'};
R = isbetween(T, 0, 1, 'OutputFormat', 'tabular');
asserts.isequal(size(R), [2 0]);
asserts.isequal(numel(R.Properties.RowNames), 2);
%=============================================================================
R = isbetween(table(), 0, 1);
asserts.isequal(size(R), [0 0]);
%=============================================================================
