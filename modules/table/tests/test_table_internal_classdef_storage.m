%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_table_internal_classdef_storage()
  T = table([1; 2], 'VariableNames', {'A'});
  asserts.isequal(class(T), 'table');
  asserts.istrue(istable(T));

  asserts.checkerror('table([1; 2], [3; 4], ''VariableNames'', {''A'', ''A''});', ...
    sprintf(_('%s names must be nonempty and distinct.'), 'Variable'));
end
%=============================================================================
