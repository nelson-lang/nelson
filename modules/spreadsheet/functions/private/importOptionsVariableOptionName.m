%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Validate a per-variable option name and return it in its canonical casing.
%=============================================================================
function canonical = importOptionsVariableOptionName(name)
  name = importOptionsScalarText(name);
  supported = {'Type', 'FillValue', 'InputFormat', 'DatetimeFormat', ...
    'DurationFormat', 'WhitespaceRule', 'EmptyFieldRule', 'TreatAsMissing', ...
    'Prefixes', 'Suffixes', 'TrimNonNumeric', 'DecimalSeparator', ...
    'ThousandsSeparator', 'Categories', 'Protected', 'QuoteRule', ...
    'ExponentCharacter', 'Name'};
  matches = strcmpi(supported, name);
  if ~any(matches)
    error('Nelson:spreadsheet:setvaropts:unknownOption', ...
      'Unrecognized variable option ''%s''.', name);
  end
  canonical = supported{find(matches, 1)};
end
%=============================================================================
