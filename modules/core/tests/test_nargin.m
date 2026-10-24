%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
addpath([nelsonroot(), '/modules/core/tests/']);
%=============================================================================
R = nargin('fun_nargin_1');
asserts.isequal(R, -3);
fun_nargin_1(1);
%=============================================================================
R = nargin('fun_nargin_2');
asserts.isequal(R, 3);
%=============================================================================
R = nargin('fun_nargin_3');
asserts.isequal(R, -1);
%=============================================================================
addpath([modulepath('functions_manager'), '/tests']);
matrix = common_functions_matrix();
for k = 1:length(matrix)
  try
    inputCount = nargin(matrix(k).name);
  catch ex
    if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()) && strcmp(ex.message, _('Function not found.'))
      continue
    end
    rethrow(ex);
  end
  asserts.istrue(isnumeric(inputCount), ['nargin failed for compatibility function: ', matrix(k).name]);
end
asserts.isequal(nargin('what'), 0);
asserts.isequal(nargin('which'), 1);
asserts.isequal(nargin('feval'), -1);
asserts.isequal(nargin('builtin'), -1);
asserts.isequal(nargin('localfunctions'), 0);
asserts.isequal(nargin('inputname'), 1);
asserts.isequal(nargin('narginchk'), 2);
%=============================================================================
% nargin on a handle to a nested function introspects the bound function
h = fun_nargin_nested();
asserts.isequal(nargin(h), 3);
%=============================================================================
