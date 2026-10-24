%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function parent = graphicsParentProperty(args)
  % Returns the target designated by a 'Parent' property pair, or [] when the
  % argument list carries none. The pair is left in place: the low level
  % constructor reads it to decide where the object is created. The last
  % occurrence wins, as a repeated property would.
  parent = [];
  for k = 1:(numel(args) - 1)
    if ischar(args{k}) && strcmpi(args{k}, 'Parent') && isscalar(args{k + 1}) ...
      && isgraphics(args{k + 1})
      parent = args{k + 1};
    end
  end
end
%=============================================================================
