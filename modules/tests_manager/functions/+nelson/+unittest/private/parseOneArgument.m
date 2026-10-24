%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [modules_list, files_to_test, option, stoponfail] = parseOneArgument(param)
  option = 'all_tests';
  stoponfail = false;
  [modules_list, files_to_test] = parseTarget(param); %#ok<NLS0004>
  if isempty(modules_list) && isempty(files_to_test)
    stoponfail = haveStopOnFailOption(param);
    if ~stoponfail
      option = getOption(param);
    end
  end
end
