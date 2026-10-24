%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% T.Properties.<name>(i) past the table dimension that <name> follows raises
% the table error; an index within it keeps the indexing error.
%=============================================================================
varId = 'Nelson:table:varIndexOutOfRange';
varMsg = 'Variable index exceeds table dimensions.';
rowId = 'Nelson:table:rowIndexOutOfRange';
rowMsg = 'Row index exceeds table dimensions.';
dimId = 'Nelson:table:dimIndexOutOfRange';
dimMsg = 'Dimension index exceeds number of table dimensions.';
T = table([1; 2], [3; 4]);
TR = table([1; 2], [3; 4], 'RowNames', {'a'; 'b'});
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames{5};
catch ex
  id = ex.identifier;
  assert_isequal(ex.message, varMsg);
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames(5);
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames{end + 1};
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames([1 5]);
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames([true true true]);
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableTypes(3);
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableUnits{3};
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = T.Properties.VariableDescriptions(5);
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = TT.Properties.VariableNames{5};
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = TT.Properties.VariableContinuity(5);
catch ex
  id = ex.identifier;
end
assert_isequal(id, varId);
%=============================================================================
id = '';
try
  v = TR.Properties.RowNames{5};
catch ex
  id = ex.identifier;
  assert_isequal(ex.message, rowMsg);
end
assert_isequal(id, rowId);
%=============================================================================
id = '';
try
  v = T.Properties.RowNames{3};
catch ex
  id = ex.identifier;
end
assert_isequal(id, rowId);
%=============================================================================
id = '';
try
  v = TT.Properties.RowTimes(3);
catch ex
  id = ex.identifier;
end
assert_isequal(id, rowId);
%=============================================================================
id = '';
try
  v = T.Properties.DimensionNames{3};
catch ex
  id = ex.identifier;
  assert_isequal(ex.message, dimMsg);
end
assert_isequal(id, dimId);
%=============================================================================
% within the table dimension: the property itself is too short
id = '';
try
  v = T.Properties.VariableUnits{2};
catch ex
  id = ex.identifier;
end
assert_isfalse(strcmp(id, varId));
assert_isfalse(isempty(id));
%=============================================================================
% not a table dimension, several subscripts, nested and aliased reads
id = '';
try
  v = T.Properties.Description(5);
catch ex
  id = ex.identifier;
end
assert_isfalse(strcmp(id, varId));
assert_isfalse(isempty(id));
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames{1, 5};
catch ex
  id = ex.identifier;
end
assert_isfalse(strcmp(id, varId));
assert_isfalse(isempty(id));
%=============================================================================
id = '';
try
  v = T.Properties.VariableNames{1}(5);
catch ex
  id = ex.identifier;
end
assert_isfalse(strcmp(id, varId));
assert_isfalse(isempty(id));
%=============================================================================
id = '';
try
  P = T.Properties;
  v = P.VariableNames{5};
catch ex
  id = ex.identifier;
end
assert_isfalse(strcmp(id, varId));
assert_isfalse(isempty(id));
%=============================================================================
id = '';
try
  s = struct(T);
  v = s.Properties.VariableNames{5};
catch ex
  id = ex.identifier;
end
assert_isequal(id, 'Nelson:badsubscript');
%=============================================================================
id = '';
try
  v = T(1, 1.5);
catch ex
  id = ex.identifier;
end
assert_isequal(id, 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals');
%=============================================================================
% reads within the bounds are unchanged
assert_isequal(T.Properties.VariableNames{2}, 'Var2');
assert_isequal(T.Properties.VariableNames(1:2), {'Var1', 'Var2'});
assert_isequal(TR.Properties.RowNames{2}, 'b');
%=============================================================================
assert_checkerror('v = T.Properties.VariableNames{5};', varMsg, varId);
assert_checkerror('v = TR.Properties.RowNames(5);', rowMsg, rowId);
assert_checkerror('v = TT.Properties.DimensionNames{5};', dimMsg, dimId);
%=============================================================================
