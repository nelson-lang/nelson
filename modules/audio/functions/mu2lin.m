%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = mu2lin(mu)
  % https://en.wikipedia.org/wiki/%CE%9C-law_algorithm
  code = 255 - mu;
  signMask = code > 127;
  segment = floor(code ./ 16);
  exponent = segment - 8 * double(signMask) + 1;
  mantissa = rem(code, 16);
  magnitude = muLawMagnitude(mantissa, exponent);
  y = applyMuLawSign(magnitude, signMask) ./ 32768;
end
%=============================================================================
function magnitude = muLawMagnitude(mantissa, exponent)
  segmentBias = [0 132 396 924 1980 4092 8316 16764];
  bias = exponent;
  bias(:) = segmentBias(exponent);
  magnitude = bias + pow2(mantissa, exponent + 2);
end
%=============================================================================
function y = applyMuLawSign(magnitude, signMask)
  y = magnitude;
  y(signMask) = -y(signMask);
end
%=============================================================================
