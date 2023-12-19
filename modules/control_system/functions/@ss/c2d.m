%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = c2d(varargin)
  % Convert model from continuous to discrete time
  % sysd = c2d(sysc, Ts)
  % sysd = c2d(sysc, Ts, method)
  % sysd = c2d(sysc, Ts, 'prewarp', w0)
  
  narginchk(2, 4);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  Ts = varargin{2};
  mustBeNumeric(Ts, 2);
  if ~isscalar(Ts);
    error(_('Wrong size for input argument #2: scalar expected.'));
  end
  
  if ~isct(sys)
    error(_('Continuous model expected.'));
  end
  if (Ts < 0)
    error(_('Ts must be finite positive scalar.'));
  end
  
  w0 = 0;
  method = 'Z';
  
  if nargin > 2
    method = toupper(convertStringsToChars(varargin{3}));
    if ~contains(method(1), {'Z', 'P', 'T'})
      error(_('Wrong value for #3 input argument: valid method expected.'));
    end
    method = method(1);
  end
  
  if nargin > 3 
    if ~strcmp(method, 'P')
      error(_("Wrong value for #3 argument: 'prewarp' method expected."));
    end
    w0 = varargin{4};
    mustBeNumeric(w0, 4);
    if ~isscalar(w0)
      error(_('Wrong size for input argument #4: scalar expected.'));
    end
    mustBePositive(w0, 4);
    mustBeFinite(w0, 4);
  end
  
  if (method == 'Z')
    sysd = c2d_zoh(sys, Ts);
  else
    if (method == 'T')
      beta = 2 / Ts;
    else
      beta = w0 / tan (w0 * (Ts / 2));
      if (w0 >= pi / Ts)
        error(_('The prewarp frequency, also known as the critical frequency, should be less than the Nyquist frequency.'));
      end
    end
    sysd = c2d_bilinear(sys, beta, Ts);
  end
  varargout{1} = sysd;
end
%=============================================================================
function sysd = c2d_zoh(sys, Ts)
  % Get info
  A = sys.A;
  B = sys.B;
  C = sys.C;
  D = sys.D;
  E = sys.E;
  % Compute sizes
  a1 = size(A, 2) + size(B, 2) - size(A, 1);
  b1 = size(A, 2);
  a2 = size(A, 2) + size(B, 2) - size(B, 1);
  b2 = size(B, 2);
  % Compute square matrix
  M = [A B; zeros(a1, b1)	zeros(a2, b2)];
  M = expm(M * Ts);
  % Find the discrete matrecies
  Ad = M(1:size(A, 1), 1:size(A, 2));
  Bd = M(1:size(B, 1), (size(A, 2) + 1):(size(A, 2) + size(B, 2)));
  sysd = ss(Ad, Bd, C, D);
  % Don't forget to add sample time
  sysd.Ts = Ts;
end
%=============================================================================
function sysd = c2d_bilinear(sys, beta, Ts)
  if isempty(sys.E)
    A = sys.A;
    B = sys.B;
    C = sys.C;
    D = sys.D;
    TYPE = 'C';
    ALPHA = 1.0;
    BETA = beta;
    [A_OUT, B_OUT, C_OUT, D_OUT, INFO] = slicot_ab04md(TYPE, ALPHA, BETA, A, B, C, D);
    if (INFO(1) ~= 0)
      error(_('slicot_ab04md fails.'));
    end
    sysd = sys;
    sysd.A = A_OUT;
    sysd.B = B_OUT;
    sysd.C = C_OUT;
    sysd.D = D_OUT;
    sysd.E = [];
    sysd.Ts = Ts;
  else
    sysd = sys;
    sysd.E = beta * sys.E - sys.A;
    sysd.D = sys.D + (sys.C / beta * sys.E - sys.A) * sys.B;
    sysd.C = sqrt(2 * beta) * (sys.C / beta * sys.E - sys.A) * sys.E;
    sysd.B = sqrt(2 * beta) * sys.B;
    sysd.A = beta * sys.E + sys.a;
    sysd.Ts = Ts;
  end
end
%=============================================================================
