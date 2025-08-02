%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = gensig(varargin)
  % Generates a signal of sin, square or pulse
  % [u, t] = gensig(type, tau)
  % [u, t] = gensig(type, tau, Tf)
  % [u, t] = gensig(type, tau, Tf, Ts)
  narginchk(2, 4);
  nargoutchk(0, 2);
  
  type = convertStringsToChars(varargin{1});
  type = lower(type);
  if length(type) < 2
    error(_("first input argument must be one of the following: 'tan', 'cos', 'sin', 'square', or 'pulse'."));
  end
  type = tolower(type(1:2));
  
  tau = varargin{2};
  Tf = [];
  
  if (nargin > 2)
    Tf = varargin{3};
  end
  if (nargin > 3)
    Ts = varargin{4};
  end
  
  if isempty(Tf) || (nargin < 3)
    Tf = 5 * tau;
  end
  if (nargin < 4)
    Ts = tau / 64;
  end
  
  if isempty(Ts) || (Ts <= 0)
    varargout{1} = zeros(0, 1);
    if nargout > 1
      varargout{2} = zeros(0, 1);
    end
    return
  end
  
  t = 0:Ts:Tf;
  t = t';
  
  switch type
    case 'pu'
      u = +(rem(t, tau) < (1 - 1000 * eps) * Ts);
    case 'sq'
      u = +(rem(t, tau) >= tau / 2);
    case 'si'
      u = sin((2 * pi / tau) * t);
    case 'co'
      u = cos((2 * pi / tau) * t);
    case 'ta'
      u = tan((2 * pi / tau) * t);
    otherwise
      error(_("first input argument must be one of the following: 'tan', 'cos', 'sin', 'square', or 'pulse'."));
    end
    varargout{1} = u;
    if nargout > 1
      varargout{2} = t;
    end
  end
