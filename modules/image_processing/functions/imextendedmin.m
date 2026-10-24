%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = imextendedmin(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  if nargin == 2
    filtered = imhmin(varargin{1}, varargin{2});
    bw = imregionalmin(filtered);
  else
    filtered = imhmin(varargin{1}, varargin{2}, varargin{3});
    bw = imregionalmin(filtered, varargin{3});
  end
end
%=============================================================================
