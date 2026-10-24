%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_assert_integer_valued(x)
  if any(~isfinite(double(x(:)))) || any(double(x(:)) ~= floor(double(x(:))))
    error('Nelson:image_processing:indexedImageValuesFiniteIntegers', 'Indexed image values must be finite integers.');
  end
end
%=============================================================================
