%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mu = lin2mu(y)
  % https://en.wikipedia.org/wiki/%CE%9C-law_algorithm
  sample = 32768 .* y;
  polarity = samplePolarity(sample);
  magnitudeCode = compressedMagnitude(abs(sample));
  mu = magnitudeCode + 64 .* polarity;
end
%=============================================================================
function polarity = samplePolarity(sample)
  polarity = sign(sample);
  polarity(sample == 0) = 1;
end
%=============================================================================
function code = compressedMagnitude(sampleMagnitude)
  clippedMagnitude = min(sampleMagnitude, 32635);
  biasedMagnitude = clippedMagnitude + 132;
  exponent = floor(log2(biasedMagnitude)) + 1;
  fraction = biasedMagnitude ./ pow2(exponent);
  code = 335 - 16 .* exponent - fix(32 .* fraction);
end
%=============================================================================
