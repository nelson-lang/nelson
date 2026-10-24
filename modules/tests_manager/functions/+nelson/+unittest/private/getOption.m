%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function option = getOption(param)
  supported_options = {'all', 'all_tests', 'unitary_tests', 'nonreg_tests', 'benchs'};
  option = [];
  if ischar(param)
    if strcmp(param, 'minimal_tests')
      option = 'unitary_tests';
    else
      found_option = any(contains(supported_options, param));
      if found_option
        option = param;
      end
    end
  end
end
%=============================================================================
