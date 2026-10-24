%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = cellstr(obj, fmt, locale)
  % Display text of each element, 'NaT' for not-a-time. The locale is
  % validated but does not change the text.
  if nargin > 2
    datetime.validateTextScalar(locale, 'Locale');
  end
  if nargin > 1
    C = cellstr(datetimeFormatText(obj, datetime.validateTextScalar(fmt, 'Format')));
  else
    C = cellstr(datetimeFormatText(obj));
  end
end
%=============================================================================
