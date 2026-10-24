%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Normalize a string scalar or character row vector into a character vector.
%=============================================================================
function text = __import_options_scalar_text__(value)
  if isstring(value)
    if ~isscalar(value)
      error('Nelson:spreadsheet:stringScalarExpected', ...
        'String scalar or character vector expected.');
    end
    text = char(value);
    return
  end
  if ischar(value) && (isrow(value) || isempty(value))
    text = value;
    return
  end
  error('Nelson:spreadsheet:stringScalarExpected', ...
    'String scalar or character vector expected.');
end
%=============================================================================
