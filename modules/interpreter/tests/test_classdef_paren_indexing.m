%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
valueObj = ClassdefParenIndexValue([11, 22, 33, 44]);
asserts.isequal(valueObj.Values(2), 22);
asserts.isequal(valueObj.Values(2:3), [22, 33]);
valueObj.Values(2) = 55;
asserts.isequal(valueObj.Values, [11, 55, 33, 44]);
valueObj.Values(2) = 22;
asserts.isequal(valueObj(2), 22);
asserts.isequal(valueObj(2:3), [22, 33]);
valueObj(3) = 99;
asserts.isequal(valueObj.Values, [11, 22, 99, 44]);
asserts.isequal(valueObj.LastValue, 99);
try
  valueObj(1, 1);
  error('Expected parenReference failure.');
catch M
  asserts.isequal(M.message, 'Unexpected parenReference.');
end
try
  valueObj(1, 1) = 5;
  error('Expected parenAssign failure.');
catch M
  asserts.isequal(M.message, 'Unexpected parenAssign.');
end
%=============================================================================
hookObj = ClassdefIndexingHooks();
asserts.isequal(hookObj(2), 5);
hookObj(2) = 50;
%=============================================================================
handleObj = ClassdefParenReferenceHandle([10, 20, 30, 40]);
asserts.isequal(handleObj(2), 20);
asserts.isequal(handleObj(2:3), [20, 30]);
handleObj(3) = 99;
asserts.isequal(handleObj.Values, [10, 20, 99, 40]);
asserts.isequal(handleObj.LastValue, 99);
delete(handleObj);
%=============================================================================
rmpath(path_test);
%=============================================================================
