%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [args, jsonOutput] = nmm_json_option(args)
  jsonOutput = false;
  if isempty(args)
    return
  end
  last = args{end};
  if ischar(last) && strcmp(last, '-json')
    jsonOutput = true;
    if length(args) == 1
      args = {};
    else
      args = args(1:length(args) - 1);
    end
  end
end
%=============================================================================
