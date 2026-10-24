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
p = path();
addpath([nelsonroot(), '/modules/core/tests/']);
%=============================================================================
R = nargout('fun_nargout_1');
asserts.isequal(R, -1);
%=============================================================================
R = nargout('fun_nargout_2');
asserts.isequal(R, -3);
%=============================================================================
R = nargout('fun_nargout_3');
asserts.isequal(R, 3);
%=============================================================================
addpath([modulepath('functions_manager'), '/tests']);
matrix = common_functions_matrix();
for k = 1:length(matrix)
  try
    outputCount = nargout(matrix(k).name);
  catch ex
    if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()) && strcmp(ex.message, _('Function not found.'))
      continue
    end
    rethrow(ex);
  end
  asserts.istrue(isnumeric(outputCount), ['nargout failed for compatibility function: ', matrix(k).name]);
end
asserts.isequal(nargout('what'), -1);
asserts.isequal(nargout('which'), 1);
asserts.isequal(nargout('feval'), -1);
asserts.isequal(nargout('builtin'), -1);
asserts.isequal(nargout('localfunctions'), 1);
asserts.isequal(nargout('inputname'), 1);
asserts.isequal(nargout('nargoutchk'), 1);
%=============================================================================
% nargout on a handle to a nested function introspects the bound function
h = fun_nargin_nested();
asserts.isequal(nargout(h), 2);
%=============================================================================
