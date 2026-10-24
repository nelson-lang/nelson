%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Records how many outputs the caller asked for, so a call site can be checked
% for the number of outputs it requests rather than for what it returns.
function varargout = nargoutprobe()
  global NARGOUT_PROBE_LAST
  NARGOUT_PROBE_LAST = nargout;
  for k = 1:nargout
    varargout{k} = k;
  end
end
%=============================================================================
