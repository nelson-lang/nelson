%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(varargin)
  onCleanupObj = varargin{1};
  currentFormat = format();
  if ~isscalar(onCleanupObj)
    h  = size(onCleanupObj, 1);
    w  = size(onCleanupObj, 2);
    header = sprintf('  %d×%d %s array with properties:', h, w, 'onCleanup');
    disp(header);
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp('   task');
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    return;
  end
  msg = sprintf(_('  %s with properties:'), 'onCleanup');
  disp(msg);
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  task = onCleanupObj.task;
  funAsStr = func2str(task);
  if ~startsWith(funAsStr, '@')
    funAsStr = ['@' funAsStr];
  end
  disp(['    task: ', funAsStr]);
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
end
%=============================================================================
