%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lotkinMatrix = lotkin(varargin)
  nSize = varargin{1};
  if nargin >= 2
    className = varargin{2};
  else
    className = 'double';
  end
  % Build Hilbert matrix and set first row to ones of the requested class
  lotkinMatrix = hilb(nSize, className);
  lotkinMatrix(1, :) = ones(1, nSize, className);
end 
%=============================================================================
