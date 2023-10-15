%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = strjust(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  if nargin == 1
    justify = 'right'; 
  else
    justify = lower(varargin{2});
  end
  mustBeMember(justify, ["left", "right", "center"], 2);
  
  str = varargin{1};
  result = str;
  for k = 1:numel(str)
    if ischar(str{k})
      result{k} = strjust(str{k}, justify);
    else
      error(_('String, cell of chars or characters vector expected.'));
    end
  end
  varargout{1} = result;
end
%=============================================================================
