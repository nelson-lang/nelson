%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function text = timetableDurationCellText(value)
  % Display text of one duration row time.
  secondsValue = seconds(value);
  switch lower(value.Format)
    case 's'
      text = [num2str(secondsValue), ' sec'];
    case 'm'
      text = [num2str(secondsValue ./ 60), ' min'];
    case 'h'
      text = [num2str(secondsValue ./ 3600), ' hr'];
    case 'd'
      text = [num2str(secondsValue ./ 86400), ' days'];
    case 'y'
      text = [num2str(secondsValue ./ (86400 * 365.2425)), ' years'];
    otherwise
      text = char(string(value));
  end
end
%=============================================================================
