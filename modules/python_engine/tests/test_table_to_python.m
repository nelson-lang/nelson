%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
R = pyrun("R = T", "R", "T", T);
assert_isequal(class(R), 'py.dict');
S = R.struct();
assert_isequal(fieldnames(S), {'data'; 'Properties'});
data = S.data.struct();
assert_isequal(fieldnames(data), {'A'; 'B'});
assert_isequal(data.A.double(), [1, 2]);
assert_isequal(data.B.double(), [3, 4]);
props = S.Properties.struct();
variableNames = props.VariableNames.cell();
assert_isequal(variableNames{1}.char(), 'A');
assert_isequal(variableNames{2}.char(), 'B');
rowNames = props.RowNames.cell();
assert_isequal(rowNames{1}.char(), 'r1');
assert_isequal(rowNames{2}.char(), 'r2');
%=============================================================================
