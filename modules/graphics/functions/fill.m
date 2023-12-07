%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fill(varargin)
  % fill(X, Y, C)
  % fill(..., Name, Value)
  % fill(ax, ...)
  % p = fill(...)

  nargoutchk(0, 1);
  narginchk(3, 100);
  
  args = parsePatchArguments(varargin);
  
  h = patch(args{:});
  
  if (nargout > 0)
    varargout{1} = h;
  else
    varargout = {};
  end
end
%=============================================================================
function args = parsePatchArguments(inputArguments)
  if (length(inputArguments) > 0 &&isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = [];
  end
  
  nbInputArguments = length(inputArguments);
  
  firstString = find (cellfun ('isclass', inputArguments, 'char'), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  if (firstString == 3)
    XData = inputArguments{1};
    YData = inputArguments{2};
    CData = validatecolor(inputArguments{3});
    ZData = zeros(size(XData));
  elseif (firstString == 4)
    XData = inputArguments{1};
    YData = inputArguments{2};
    CData = inputArguments{3};
    ZData = zeros(size(XData));
  elseif (nbInputArguments > 0) 
    error(_('Wrong number of input arguments.'));
  end
  args = {};
  if ~isempty(ax)
    args = {ax};
  end
  args = [args, XData, YData, ZData, CData, inputArguments(firstString+1:end)];
end
%=============================================================================
