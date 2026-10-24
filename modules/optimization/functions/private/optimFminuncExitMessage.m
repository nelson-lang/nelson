%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function msg = optimFminuncExitMessage(exitflag, defaultMessage)
  if exitflag == 1
    msg = _('Local minimum found.');
  elseif exitflag == 2
    msg = _('Step size tolerance reached.');
  elseif exitflag == 3
    msg = _('Function tolerance reached.');
  elseif exitflag == -1
    msg = _('Stopped by output function.');
  elseif exitflag == -3
    msg = _('Objective limit reached.');
  else
    msg = defaultMessage;
  end
end
%=============================================================================
