%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isfir(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin == 1
    a = 1;
  else
    a = varargin{2};
  end
  a = a(:).';
  tf = isempty(a) || length(a) == 1 || all(a(2:end) == 0);
end
%=============================================================================
