%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = hist(varargin)
  
  narginchk(1, 100);
  nargoutchk(0, 2);
  
  inputArguments = varargin;
  nbInputArguments = length(inputArguments);
  if (nargout == 0)
    if (nbInputArguments >= 1)
      if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
        parent = inputArguments{1}(1);
        inputArguments = inputArguments(2:end);
        nbInputArguments = nbInputArguments - 1;
      else
        parent = newplot();
      end
    else
      parent = newplot();
    end
  end
  
  y = inputArguments{1};
  inputArguments = inputArguments(2:end);
  nbInputArguments = length(inputArguments);
  
  asVector = isvector(y);
  y = y(:);
  
  if ~isreal(y)
    y = real(y);
  end 
  max_val = max (y(:));
  min_val = min (y(:));
  
  if (nbInputArguments == 0)
    n = 10;
    x = [0.5:n]'/n;
    x = x * (max_val - min_val) + ones(size(x)) * min_val;
  else
    x = inputArguments{1};
    inputArguments = inputArguments(2:end);
    if (isscalar(x))
      n = x;
      if (n <= 0)
        error(_('Second argument must be positive.'));
      end
      x = [0.5:n]'/n;
      x = x * (max_val - min_val) + ones (size (x)) * min_val;
    elseif (isreal (x))
      if (isvector (x))
        x = x(:);
      end
      x = sort (x);
    else
      error(_('Second argument must be a scalar or vector.'));
    end
  end
  
  x = double (x);
  y = double (y);
  freq = computesFreq(x, y);
  
  if (nbInputArguments > 2 && ~ischar(inputArguments{iarg}))
    norm = inputArguments{iarg};
    inputArguments = inputArguments(2:end);
    freq = freq / size(y, 1) * norm;
  end
  
  if nargout == 0
    if (size(freq, 2) == 1)
      width = 1;
    else
      width = 0.8;
    end
    h = bar(parent, x, freq, width, inputArguments{:});
  end
  
  if nargout > 0
    if ~asVector
      varargout{1} = freq;
    else
      varargout{1} = freq';
    end
  end
  
  if nargout > 1
    if ~asVector
      varargout{2} = x;
    else
      varargout{2} = x';
    end
  end
end
%=============================================================================
function freq = computesFreq(x, y)
  cutout = (x(1:end - 1,:) + x(2:end, :)) / 2;
  n = size(x, 1);
  offset = 30;
  withLoopOnN = (n < offset && size(x, 2) == 1);
  if ~withLoopOnN
    [s, idx] = sort([y; cutout]);
    lenY = size(y, 1);
    histValues = cumsum(idx <= lenY);
    x1 = zeros(1, size(y, 2));
    x2 = reshape(histValues(idx > lenY), size(cutout));
    x3 = histValues(end, :) - sum(isnan(y));
    histValues = [x1; x2; x3];
  else
    histValues = zeros (n + 1, size(y, 2));
    for i = 1:n-1
      histValues(i + 1, :) = sum(y <= cutout(i));
    end
    histValues(n + 1, :) = sum(~isnan(y));
  end
  freq = diff(histValues);
end
%=============================================================================
