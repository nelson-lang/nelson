%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function launcher = progressive_case_launcher(testCase)
  if testCase.options.gui_mode
    launcher = 'gui';
  elseif testCase.options.adv_cli_mode
    launcher = 'adv_cli';
  else
    launcher = 'cli';
  end
end
%=============================================================================
