%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = applyDisplaySkipPolicy(test_case)
  requiresDisplay = test_case.options.gui_mode || test_case.options.with_display ...
    || test_case.options.adv_cli_mode;
  if (~requiresDisplay)
    return
  end
  if ~haveDisplay()
    test_case = markSkipped(test_case, 'No display');
  elseif isRemoteDisplaySession() && ~canUseOffscreenQmlDisplay(test_case)
    test_case = markSkipped(test_case, 'Remote display session');
  end
end
%=============================================================================
function tf = canUseOffscreenQmlDisplay(test_case)
  tf = ismac() && endsWith(test_case.classname, '.qml_engine') ...
    && any(strcmp(getenv('QT_QPA_PLATFORM'), {'offscreen', 'minimal'}));
end
%=============================================================================
