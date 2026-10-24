%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = haveDisplay()
  if isTruthyEnvironmentValue('NELSON_TESTS_FORCE_DISPLAY')
    res = true;
    return
  end
  if any(strcmp(getenv('QT_QPA_PLATFORM'), {'offscreen', 'minimal'}))
    res = true;
    return
  end
  persistent withDisplay %#ok<NLS0005>
  if isempty(withDisplay)
    withDisplay = true;
    if ~ispc()
      if ismac()
        [status, output] = system('/usr/bin/pgrep -x WindowServer');
        withDisplay = (status == 0) && ~isempty(strtrim(output));
      else
        DISPLAY_ENV = getenv('DISPLAY');
        withDisplay = ~strcmp(DISPLAY_ENV, '');
      end
    end
  end
  res = withDisplay;
end
