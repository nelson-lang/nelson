%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ax, args] = polarAxesForFunction(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'polaraxes'))
    ax = args{1};
    args(1) = [];
  else
    ax = gca();
  end
  if ~polarIsAxes(ax)
    error('Nelson:polarAxesForFunction:polarAxesExpected', 'Polar axes expected.');
  end
end
%=============================================================================
