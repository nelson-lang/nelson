%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ss(varargin)
  % Generates a state space model from matrix A, B, C, D
  % Input: A, B, C, D, Ts
  % sys = ss(A, B, C, D)
  % sys = ss(A, B, C, D, Ts)
  % sys = ss(D)
  
  nargoutchk(0, 1);
  
  sys = [];
  if (nargin == 0)
    sys = ss_no_rhs();
  end
  if (nargin == 1)
    sys = ss_one_rhs(varargin{1});
  end
  if (nargin == 4 || nargin == 5)
    Ts = 0;
    if (nargin == 5)
      Ts = varargin{5};
      isValidTs = isempty(Ts) || (isscalar(Ts) && (Ts == -1 || Ts >= 0));
      if ~isValidTs
        error(_('Ts property should be either a positive scalar, 0, or -1 to indicate that it is unspecified.'));
      end
    end
    A = varargin{1};
    B = varargin{2};
    C = varargin{3};
    D = varargin{4};
    sys = ss_ABCD(A, B, C, D, Ts);
  end
  if (~isa(sys, 'ss'))
    error(_('Wrong number of input arguments.'));
  end
  varargout{1} = sys;
end
%=============================================================================
function sys = ss_no_rhs()
  ss = {};
  ss.A = [];
  ss.B = [];
  ss.C = [];
  ss.D = [];
  ss.E = [];
  ss.Scaled = false;
  ss.Ts = 0;
  ss.Internal = [];
  ss.Internal.Version = 1;
  ss.Internal.Ts = 0;
  ss.TimeUnit = 'seconds';
  ss.UserData = [];
  sys = class(ss, 'ss');
end
%=============================================================================
function sys = ss_one_rhs(D)
  sys = ss_no_rhs();
  sys.B = zeros(0, size(D, 2));
  sys.C = zeros(size(D, 1),0);
  sys.D = D;
  sys.Internal.Ts = -2;
end
%=============================================================================
function sys = ss_ABCD(A, B, C, D, Ts)
  [A, B, C, D, Ts] = adapt_ABCD(A, B, C, D, Ts);
  msg = checkABCDE(A, B, C, D);
  
  if ~isempty(msg)
    error(msg.identifier, msg.message);
  end
  
  sys = ss_no_rhs();
  
  sys.A = A;
  sys.B = B;
  sys.C = C;
  sys.D = D;
  
  if isequal(Ts, -1)
    sys.Ts = 0;
    sys.Internal.Ts = -1;
  elseif isequal(Ts, -2)
    sys.Ts = 0;
    sys.Internal.Ts = -2;
  else
    sys.Ts = Ts;
    sys.Internal.Ts = Ts;
  end
end
%=============================================================================
function [A, B, C, D, Ts] = adapt_ABCD(A, B, C, D, Ts)
  isDZero = (isscalar(D) && D(1) == 0);
  if isempty(A) && isempty(B) && isempty(C)
    [m, n] = size(D);
    A = [];
    B = zeros(0, n);
    C = zeros(m, 0);
    Ts = -2;
    return;
  end
  new_row_for_C_or_D = max(size(C, 1), size(D, 1));
  if isempty(C)
    C = eye(new_row_for_C_or_D, size(A, 1));
  end
  if (isempty(D) || isDZero)
    D = zeros(new_row_for_C_or_D, size(B, 2));
  end
end
%=============================================================================
