%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cauchy(varargin)
  narginchk(2, 3);
  
  xSeq = varargin{1};
  ySeq = varargin{2};
  if nargin < 3 || isempty(varargin{3})
    className = 'double';
  else
    classArg = varargin{3};
    if ischar(classArg) || (isstring(classArg) && isscalar(classArg))
      className = char(classArg);
    else
      className = class(classArg);
    end
  end
  if isscalar(xSeq)
    xSeq = 1:xSeq;
  end
  if isscalar(ySeq)
    ySeq = 1:ySeq;
  elseif isempty(ySeq)
    ySeq = xSeq;
  end
  
  xColumn = xSeq(:);
  yColumn = ySeq(:); 
  num = cast(1, className);
  den = xColumn + yColumn.';
  varargout{1} = num ./ den;
end
%=============================================================================
