%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The call sites live in a function body, which is compiled the first time the
% function runs: by then +lowerpkg is on the path, so the package call is
% resolved when it is compiled. That is the case the test is about; a script
% compiled before its own addpath() resolves the same calls at run time.
function [bare, terminated, assigned, value] = packageNargoutHelper()
  global NARGOUT_PROBE_LAST
  NARGOUT_PROBE_LAST = -1;
  lowerpkg.nargoutprobe()
  bare = NARGOUT_PROBE_LAST;
  NARGOUT_PROBE_LAST = -1;
  lowerpkg.nargoutprobe();
  terminated = NARGOUT_PROBE_LAST;
  NARGOUT_PROBE_LAST = -1;
  value = lowerpkg.nargoutprobe();
  assigned = NARGOUT_PROBE_LAST;
end
%=============================================================================
