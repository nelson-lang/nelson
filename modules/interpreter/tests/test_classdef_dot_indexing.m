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
valueObj = ClassdefDotIndexValue(30);
asserts.isequal(valueObj.Stored, 30);
asserts.isequal(valueObj.virtual, 31);
valueObj.virtual = 41;
asserts.isequal(valueObj.Stored, 40);
asserts.isequal(valueObj.LastName, 'virtual');
asserts.isequal(valueObj.LastValue, 41);
try
  valueObj.missing;
  error('Expected dotReference failure.');
catch M
  asserts.isequal(M.message, 'Unexpected dotReference.');
end
try
  valueObj.missing = 1;
  error('Expected dotAssign failure.');
catch M
  asserts.isequal(M.message, 'Unexpected dotAssign.');
end
%=============================================================================
handleObj = ClassdefDotIndexHandle(50);
asserts.isequal(handleObj.Stored, 50);
asserts.isequal(handleObj.virtual, 52);
handleObj.virtual = 62;
asserts.isequal(handleObj.Stored, 60);
asserts.isequal(handleObj.LastName, 'virtual');
asserts.isequal(handleObj.LastValue, 62);
try
  handleObj.missing;
  error('Expected dotReference failure.');
catch M
  asserts.isequal(M.message, 'Unexpected dotReference.');
end
try
  handleObj.missing = 1;
  error('Expected dotAssign failure.');
catch M
  asserts.isequal(M.message, 'Unexpected dotAssign.');
end
delete(handleObj);
%=============================================================================
rmpath(path_test);
%=============================================================================
