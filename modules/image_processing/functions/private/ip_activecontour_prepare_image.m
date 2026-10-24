%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function image2d = ip_activecontour_prepare_image(img)
  ip_assert_full_image(img, true, true);
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  s = size(img);
  if length(s) < 3
    image2d = img;
    return
  end
  r = ip_to_unit_double(img(:, :, 1));
  g = ip_to_unit_double(img(:, :, 2));
  b = ip_to_unit_double(img(:, :, 3));
  image2d = 0.2989360213 * r + 0.5870430745 * g + 0.1140209043 * b;
end
%=============================================================================
