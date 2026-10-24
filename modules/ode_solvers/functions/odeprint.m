%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function status = odeprint(t, y, flag, varargin)
  hasNoFlag = nargin < 3 || isempty(flag);
  if hasNoFlag
    t
    y
  else
    if isstring(flag) || ischar(flag)
      activeFlag = char(flag);
    else
      activeFlag = flag;
    end
    if strcmp(activeFlag, 'init')
      disp('t =')
      disp(t(1))
      y
    elseif strcmp(activeFlag, 'done')
      fprintf('\n\n');
    end
  end
  status = 0;
end
%=============================================================================
