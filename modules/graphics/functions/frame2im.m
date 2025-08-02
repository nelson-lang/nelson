%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = frame2im(varargin)
  % RGB = frame2im(F)
  % [X, map] = frame2im(F)
  narginchk(1, 1);
  nargoutchk(0, 2);
  F = varargin{1};
  mustBeA(varargin{1}, 'struct');
  if (~isequal(fieldnames(F), {'cdata';'colormap'}))
    error('Nelson:frame2im:invalidInput', _('Invalid structure fields.'));
  end
  n = numel(F);
  X = [];
  map = [];
  if (n > 0)
    x = [F.cdata];
    map(1) = F(1).colormap;
  end
  varargout{1} = x;
  if nargout > 1
    varargout{2} = map;
  end
end
%=============================================================================
