%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [texts, ok] = tableDisplayNumberText(values)
  % Text of each element of a numeric matrix shown in a table, same size as
  % values: the shortest form with 5 significant digits (15 in format long,
  % 7 for single), integers in full; complex numbers get real and imaginary
  % parts aligned. ok is false for the fixed-point formats
  % (shortE, longE, bank, longEng), which keep the matrix element display.
  texts = strings(size(values));
  ok = true;
  if isinteger(values)
    texts = string(values);
    return
  end
  currentFormat = format();
  switch currentFormat.NumericFormat
    case {'shortE', 'longE', 'bank', 'longEng'}
      ok = false;
      return
    case {'long', 'longG'}
      digits = 15;
      if isa(values, 'single')
        digits = 7;
      end
    otherwise
      digits = 5;
  end
  if isempty(values)
    return
  end
  values = full(values);
  % -0 is shown as 0.
  values(values == 0) = 0;
  spec = sprintf('%%.%dg', digits);
  texts = reshape(compose(spec, real(values)), size(values));
  if ~isreal(values)
    % Real parts right-aligned, imaginary parts left-aligned.
    imagTexts = reshape(compose(sprintf('%%+.%dg', digits), imag(values)), size(values));
    texts = pad(texts, max(strlength(texts(:))), 'left') ...
      + pad(imagTexts + "i", max(strlength(imagTexts(:))) + 1, 'right');
  end
end
%=============================================================================
