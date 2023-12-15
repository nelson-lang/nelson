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
function varargout = gram(varargin)
  % Wc = gram(sys, 'c')
  % Wc = gram(sys, 'o')
  narginchk(2, 2);
  nargoutchk(0, 1);
  option = varargin{2};
  option = tolower(convertStringsToChars(option));
  
  sys = varargin{1};
  
  % Get sample time
  sampleTime = sys.Ts;
  
  if(sampleTime > 0)
    % Discrete way
    A = sys.A;
    B = sys.B;
    C = sys.C;
    
    if(strcmp(option, 'c'))
      X = dlyap(A, B * B');
    elseif(strcmp(option, 'o'))
      X = dlyap(A', C' * C);
    else
      error(_("'o', 'c' second argument expected."));
    end
  else
    % Continous way
    A = sys.A;
    B = sys.B;
    C = sys.C;
    
    if (strcmp(option, 'c'))
      X = lyap(A, B*B');
    elseif (strcmp(option, 'o'))
      X = lyap(A', C' * C);
    else
      error(_("'o', 'c' second argument expected."));
    end
  end
  varargout{1} = X;
end
