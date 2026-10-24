%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [sn, cn, dn] = signalEllipj(u, m)
  k = signalEllipke(m);
  kp = signalEllipke(1 - m);
  q = exp(-pi * kp / k);
  v = pi * u / (2 * k);
  theta20 = signalTheta2(0, q);
  theta30 = signalTheta3(0, q);
  theta40 = signalTheta4(0, q);
  theta4v = signalTheta4(v, q);
  sn = (theta30 / theta20) * signalTheta1(v, q) ./ theta4v;
  cn = (theta40 / theta20) * signalTheta2(v, q) ./ theta4v;
  dn = (theta40 / theta30) * signalTheta3(v, q) ./ theta4v;
end
%=============================================================================
function y = signalTheta1(v, q)
  y = zeros(size(v));
  for n = 0:40
    term = 2 * ((-1) ^ n) * q ^ ((n + 0.5) ^ 2) * sin((2 * n + 1) * v);
    y = y + term;
    if max(abs(term)) <= 1e-15
      break;
    end
  end
end
%=============================================================================
function y = signalTheta2(v, q)
  y = zeros(size(v));
  for n = 0:40
    term = 2 * q ^ ((n + 0.5) ^ 2) * cos((2 * n + 1) * v);
    y = y + term;
    if max(abs(term)) <= 1e-15
      break;
    end
  end
end
%=============================================================================
function y = signalTheta3(v, q)
  y = ones(size(v));
  for n = 1:40
    term = 2 * q ^ (n ^ 2) * cos(2 * n * v);
    y = y + term;
    if max(abs(term)) <= 1e-15
      break;
    end
  end
end
%=============================================================================
function y = signalTheta4(v, q)
  y = ones(size(v));
  for n = 1:40
    term = 2 * ((-1) ^ n) * q ^ (n ^ 2) * cos(2 * n * v);
    y = y + term;
    if max(abs(term)) <= 1e-15
      break;
    end
  end
end
%=============================================================================
