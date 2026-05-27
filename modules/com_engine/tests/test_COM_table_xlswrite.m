%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--EXCEL REQUIRED-->
%=============================================================================
filename = [tempdir(), 'test_COM_table_xlswrite.xlsx'];
if isfile(filename)
  rmfile(filename);
end
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
r = COM_xlswrite(filename, T, 'TableData');
assert_istrue(r);
[numericData, textData, rawData] = COM_xlsread(filename, 'TableData', 'A1:B3');
assert_isequal(textData, {'A', 'B'});
assert_isequal(numericData, [1 3; 2 4]);
assert_isequal(rawData, {'A', 'B'; 1, 3; 2, 4});
%=============================================================================
