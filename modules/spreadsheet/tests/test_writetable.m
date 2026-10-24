%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Delimited text table export is not stable in the portable runtime yet.');
%=============================================================================
T = table([1; 2], {'A'; 'B'}, 'VariableNames', {'Id', 'Name'});
filename = [tempname(), '.csv'];
writetable(T, filename);
R = readtable(filename);
asserts.isequal(R.Id, T.Id);
asserts.isequal(R.Name, T.Name);
rmfile(filename);
%=============================================================================
