%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_filter_along_dim3(data, kernel, dim, padding)
  s = size(data);
  out = zeros(s);
  radius = (numel(kernel) - 1) / 2;
  for k = 1:numel(kernel)
    shift = k - radius - 1;
    out = out + kernel(k) * ip_shift_volume_dim3(data, dim, shift, padding);
  end
end
%=============================================================================
function shifted = ip_shift_volume_dim3(data, dim, shift, padding)
  s = size(data);
  if ischar(padding)
    shifted = ip_shift_volume_indexed(data, dim, shift, padding, s);
  else
    shifted = zeros(s) + padding;
    shifted = ip_shift_volume_constant(data, shifted, dim, shift, s);
  end
end
%=============================================================================
function shifted = ip_shift_volume_indexed(data, dim, shift, padding, s)
  if dim == 1
    idx = ip_pad_index((1:s(1)) + shift, s(1), padding);
    shifted = data(idx, :, :);
  elseif dim == 2
    idx = ip_pad_index((1:s(2)) + shift, s(2), padding);
    shifted = data(:, idx, :);
  else
    idx = ip_pad_index((1:s(3)) + shift, s(3), padding);
    shifted = data(:, :, idx);
  end
end
%=============================================================================
function shifted = ip_shift_volume_constant(data, shifted, dim, shift, s)
  if dim == 1
    idx = (1:s(1)) + shift;
    valid = idx >= 1 & idx <= s(1);
    shifted(valid, :, :) = data(idx(valid), :, :);
  elseif dim == 2
    idx = (1:s(2)) + shift;
    valid = idx >= 1 & idx <= s(2);
    shifted(:, valid, :) = data(:, idx(valid), :);
  else
    idx = (1:s(3)) + shift;
    valid = idx >= 1 & idx <= s(3);
    shifted(:, :, valid) = data(:, :, idx(valid));
  end
end
%=============================================================================
