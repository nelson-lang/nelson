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
obj = ClassdefIndexingHooks();
asserts.isequal(obj(2), 5);
asserts.isequal(obj(2:3), [5, 6]);
asserts.isequal(obj(end), 7);
asserts.isequal(obj.first, 4);
%=============================================================================
obj(2) = 50;
asserts.isequal(obj(2), 5);
obj.first = 40;
asserts.isequal(obj.first, 4);
%=============================================================================
try
  obj.missing;
  error('Expected subsref failure.');
catch M
  asserts.isequal(M.message, 'Unexpected subsref.');
end
try
  obj.missing = 1;
  error('Expected subsasgn failure.');
catch M
  asserts.isequal(M.message, 'Unexpected subsasgn.');
end
%=============================================================================
