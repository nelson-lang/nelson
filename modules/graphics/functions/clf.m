%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = clf(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if nargin == 1
    f = varargin{1};
  else
    f = gcf();
  end
  f.Children = [];
  f.CurrentAxes = [];
  refresh(f);
  if nargout == 1
    varargout{1} = f;
  else
    varargout = {};
  end
end
%=============================================================================
