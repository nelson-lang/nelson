%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [parent, args] = graphicsExtractParentProperty(args)
  % Pulls the target designated by a 'Parent' property pair out of the argument
  % list, returning [] when there is none. Use this rather than
  % graphicsParentProperty when the remaining arguments are forwarded to another
  % high level plotting function: left in place, the pair would make that call
  % start a new plot of its own and wipe the objects just created.
  % The last occurrence wins, as a repeated property would.
  parent = [];
  k = 1;
  while (k < numel(args))
    if ischar(args{k}) && strcmpi(args{k}, 'Parent') && isscalar(args{k + 1}) ...
      && isgraphics(args{k + 1})
      parent = args{k + 1};
      args(k:k + 1) = [];
    else
      k = k + 1;
    end
  end
end
%=============================================================================
