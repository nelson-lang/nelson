%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
valueObj = ClassdefBraceIndexValue({11, 22, 33});
asserts.isequal(valueObj.Values{2}, 22);
asserts.isequal(valueObj{2}, 22);
valueObj{3} = 99;
asserts.isequal(valueObj.Values, {11, 22, 99});
asserts.isequal(valueObj.LastValue, 99);
try
  valueObj{1, 1};
  error('Expected braceReference failure.');
catch M
  asserts.isequal(M.message, 'Unexpected braceReference.');
end
try
  valueObj{1, 1} = 5;
  error('Expected braceAssign failure.');
catch M
  asserts.isequal(M.message, 'Unexpected braceAssign.');
end
%=============================================================================
handleObj = ClassdefBraceIndexHandle({10, 20, 30});
asserts.isequal(handleObj{2}, 20);
handleObj{3} = 77;
asserts.isequal(handleObj.Values, {10, 20, 77});
asserts.isequal(handleObj.LastValue, 77);
delete(handleObj);
%=============================================================================
rmpath(path_test);
%=============================================================================
