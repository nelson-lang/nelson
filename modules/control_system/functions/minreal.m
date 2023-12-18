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
function varargout = minreal(varargin)
  % Minimal realization and pole-zero cancellation.
  % [a, b, c, d] = minreal(a, b, c, d)
  % [a, b, c, d] = minreal(a, b, c, d, tol)
  narginchk(4, 5);
  nargoutchk(0, 4);
  [msg, A, B, C, D] = abcdchk(varargin{:});
  if ~isempty(msg)
    error(msg.id, msg.message);
  end
  
  if nargin < 5
    tol = 1e-6;
  else
    tol = varargin{5};
  end
  
  [ns, nu] = size(B);
  if nargin < 5
    tol = 10 * ns * norm(A, 1) * eps;
  else
    tol = varargin{5};
  end
  
  [am, bm, cm, nn] = reduceSystem(A, B, C, ns, tol);
  if nn > 1
    msg = sprintf(_('%d states removed.'), nn);
  else
    msg = sprintf(_('%d state removed.'), nn);
  end 
  disp(msg);
  
  varargout{1} = am;
  if nargout > 1
    varargout{2} = bm;
  end
  if nargout > 2
    varargout{3} = cm;
  end
  if nargout > 3
    varargout{4} = D;
  end
end
%=============================================================================
function [am, bm, cm, nn] = reduceSystem(A, B, C, ns, tol)
  [am, bm, cm, t, k] = ctrbf(A, B, C, tol);
  kk = sum(k);
  nn = ns - kk;
  nu = nn;
  idx = (nu + 1):ns;
  am = am(idx, idx);
  bm = bm(idx, :);
  cm = cm(:, idx);
  ns = ns - nu;
  if ns
    [am, bm, cm, t, k] = obsvf(am, bm, cm, tol);
    kk = sum(k);
    nu = ns - kk;
    nn = nn + nu;
    idx = (nu + 1):ns;
    am = am(idx, idx);
    bm = bm(idx, :);
    cm = cm(:, idx);
  end
end
%=============================================================================
