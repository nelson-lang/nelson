%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [args, quiet] = nmm_quiet_option(args)
  quiet = false;
  if length(args) < 2
    return
  end
  last = args{end};
  if ischar(last) && strcmp(last, '-quiet')
    quiet = true;
    args = args(1:length(args) - 1);
  end
end
%=============================================================================
