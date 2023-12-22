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
function varargout = freqresp(varargin)
  % Computes the frequency response from a transfer function or state space model
  % [H, wout] = freqresp(sys, w)
  % H = freqresp(sys, w)
  
  narginchk(2, 2);
  nargoutchk(0, 2);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  
  Ts = sys.Ts;
  w = varargin{2};
  sys = tf(sys);
  L = length(w);	% Number of frequency elements
  H = zeros(size(sys, 1), size(sys, 2), L);
  delay = 0;
  for i = 1:size(sys, 1)
    for j = 1:size(sys, 2)
      % Get numerator vector and denomerator vector
      a = sys(i, j).Numerator{1};
      b = sys(i, j).Denominator{1};
      
      % Numerator and denomerator need to be the same length
      if(length(a) > length(b))
        b = [zeros(1, size(a,2) - size(b,2)) b];
      elseif(length(a) < length(b))
        a = [zeros(1, size(b,2) - size(a,2)) a];
      end
      
      N = length(b); % Number of denomerators
      wout = w;
      % Evaluate transfer function
      h = Ts;
      if (Ts > 0) % Discrete model
        for k = 1 : L
          H(i, j, k) = (a*fliplr((exp(1i*wout(k)*h)).^(0 : N-1)).')/(b*fliplr((exp(1i*wout(k)*h)).^(0 : N-1)).')*exp(-delay*exp(1i*wout(k)*h));
        end
      else % Continous
        for k = 1 : L
          H(i, j, k) = (a*fliplr((1i*wout(k)).^(0 : N-1)).')/(b*fliplr((1i*wout(k)).^(0 : N-1)).')*exp(-delay*1i*wout(k));
        end
      end
    end
  end
  varargout{1} = H;
  if nargout > 1
    varargout{2} = wout;
  end
end
