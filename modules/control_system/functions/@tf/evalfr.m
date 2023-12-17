%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = evalfr(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  G = varargin{1};
  w = varargin{2};
  H = zeros(size(G,1),size(G,2), 1);
  delay = 0;
  for i = 1:size(G,1)
    for j = 1:size(G,2)
      % Get numerator vector and denomerator vector
      a = G(i,j).Numerator{1};
      b = G(i,j).Denominator{1};
      
      % Numerator and denomerator need to be the same length
      if(length(a) > length(b))
        b = [zeros(1, size(a,2) - size(b,2)) b];
      elseif(length(a) < length(b))
        a = [zeros(1, size(b,2) - size(a,2)) a];
      end
      
      % Number of denomerators
      N = length(b);
      % Evaluate transfer function
      for k = 1 : 1
        H(i, j, k) = (a * fliplr((w(k)) .^ (0 : N-1)) .' )/(b * fliplr((w(k)) .^ (0 : N-1)).') * exp(-delay * w(k));
      end
      % Done!
    end
  end
  varargout{1} = H;
end
