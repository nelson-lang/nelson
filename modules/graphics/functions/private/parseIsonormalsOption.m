%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [args, negate] = parseIsonormalsOption(args)
  negate = false;
  if isempty(args) || ~ischar(args{end})
    return
  end
  if strcmpi(args{end}, 'negate')
    negate = true;
    args(end) = [];
  else
    error('Nelson:parseIsonormalsOption:invalidIsonormalsOption', 'Invalid isonormals option.');
  end
end
%=============================================================================
