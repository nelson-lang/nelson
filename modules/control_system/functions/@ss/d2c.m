%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = d2c(varargin)
  
  narginchk(1, 3);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  if ~isdt(sys)
    error(_('Discret model expected.'));
  end
  if ~issiso(sys)
    error(_('SISO LTI model expected.'));
  end
  
  w0 = 0;
  method = 'Z';
  
  if nargin > 1
    method = toupper(convertStringsToChars(varargin{2}));
    if ~contains(method(1), {'Z', 'P', 'T'})
      error(_('Wrong value for #3 input argument: valid method expected.'));
    end
    method = method(1);
  end
  if nargin > 2 
    if ~strcmp(method, 'P')
      error(_("Wrong value for #2 argument: 'prewarp' method expected."));
      end
      w0 = varargin{3};
      mustBeNumeric(w0, 3);
      if ~isscalar(w0)
        error(_('Wrong size for input argument #3: scalar expected.'));
      end
      mustBePositive(w0, 3);
      mustBeFinite(w0, 3);
    end
    
    if strcmp(method, 'Z')
      sysc = d2c_zoh(sys);
    else
      Ts = sys.Ts;
      if strcmp(method, 'P')
        beta = w0 / tan (w0 * Ts/2);
      else % 'T'
        beta = 2 / Ts;
      end
      
      if isempty(sys.E)
        sysc = d2c_prewarp(sys, beta);
      else
        sysc = d2c_bilinear(sys, beta)
      end
    end
    varargout{1} = sysc;
  end
  %=============================================================================
function sysc = d2c_zoh(sys)
  A = sys.A;
  B = sys.B;
  C = sys.C;
  D = sys.D;
  Ts = sys.Ts;
  % Compute sizes
  a1 = size(A, 2) + size(B, 2) - size(A, 1);
  b1 = size(A, 2);
  a2 = size(A, 2) + size(B, 2) - size(B, 1);
  b2 = size(B, 2);
  % Compute square matrix
  M = [A B; zeros(a1, b1)  eye(a2, b2)];
  M = round(logm(M) * (1/Ts) * (1 / 1e-6)) * 1e-6; % Very important to remove small numbers!!
  M(abs(M) < 1e-6) = 0; % Very importat to turn -0 to 0.
  A = M(1:size(A, 1), 1:size(A, 2));
  B = M(1:size(B, 1), (size(A, 2) + 1):(size(A, 2) + size(B, 2)));
  % Return model
  sysc = ss(A, B, C, D);
  % Don't forget to add sample time to zero again
  sysc.Ts = 0;
  sysc.E = sys.E;
end
%=============================================================================
function sysc = d2c_prewarp(sys, beta)
  TYPE = 'D';
  ALPHA = 1.0;
  BETA = beta;
  [A_OUT, B_OUT, C_OUT, D_OUT, INFO] = slicot_ab04md(TYPE, ALPHA, BETA, sys.A, sys.B, sys.C, sys.D);
  if (INFO(1) ~= 0)
    error(_('slicot_ab04md fails.'));
  end
  sysc = ss(A_OUT, B_OUT, C_OUT, D_OUT);
  sysc.E = [];
end
%=============================================================================
function sysc = d2c_bilinear(sys, beta)
  sqrtBeta = sqrt(2 * beta); 
  E = sys.E + sys.A;
  D = sys.D - (sys.C / sys.E + sys.A) * sys.B;
  C = sqrtBeta * (sys.C / sys.E + sys.A) * sys.E;
  B = sqrtBeta * sys.B;
  A = beta * (sys.A - sys.E);
  sysc = ss(A, B, C, D, 0);
  sysc.E = E; 
end
%=============================================================================
