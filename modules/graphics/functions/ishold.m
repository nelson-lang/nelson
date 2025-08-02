%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = ishold(varargin)
  narginchk(0, 1);
  if nargin == 1
    a = varargin{1};
  else
    a = gca();
  end
  tf = strcmp(a.NextPlot, 'add');
end
%=============================================================================
