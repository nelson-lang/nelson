%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pascal(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  nSize = varargin{1};
  mustBePositive(nSize, 1);
  mustBeInteger(nSize, 1);
  if nargin < 2 || isempty(varargin{2})
    kind = 0;
  else
    kind = varargin{2};
  end
  if nargin < 3 || isempty(varargin{3})
    className = 'double';
  else
    className = varargin{3};
  end
  if nargin == 2 && (ischar(kind) || (isstring(kind) && isscalar(kind)))
    className = kind;
    kind = 0;
  elseif nargin < 3
    className = 'double';
  end
  if isstring(className) && isscalar(className)
    className = char(className);
  end
  if nargin < 2 || isempty(kind)
    kind = 0;
  end
  PascalMatrix = diag((-1).^(0:cast(nSize,className)-1));
  PascalMatrix(:,1) = ones(nSize,1,className);
  
  for col = 2:nSize-1
    for row = col+1:nSize
      PascalMatrix(row,col) = PascalMatrix(row-1,col) - PascalMatrix(row-1,col-1);
    end
  end
  
  if kind == 0
    PascalMatrix = PascalMatrix*PascalMatrix';
  elseif kind == 2
    PascalMatrix = rot90(PascalMatrix,3);
    if rem(nSize,2) == 0
      PascalMatrix = -PascalMatrix;
    end
  elseif kind ~= 1
    error(_('Wrong value for #2 argument. 0, 1, or 2 expected.'));
  end
  varargout{1} = cast(PascalMatrix, className);
end
%=============================================================================
