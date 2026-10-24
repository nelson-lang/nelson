%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function modules = getModulesToTest(param) %#ok<NLS0026>
  modules = [];
  if iscellstr(param)
    for k = param(:)'
      if ismodule(k{1})
        if isempty(modules)
          modules = k;
        else
          modules = [modules; k{1}];
        end
      end
    end
  elseif ischar(param)
    if ismodule(param)
      modules = {param};
    elseif strcmp(param, 'minimal_tests')
      run([nelsonroot(), '/modules/tests_manager/minimal_tests.m']);
      modules = minimal_tests;
    end
  end
end
