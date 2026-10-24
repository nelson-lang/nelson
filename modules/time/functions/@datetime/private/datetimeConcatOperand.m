%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = datetimeConcatOperand(v)
  % numbers are not dates: only datetimes, date text and [] concatenate
  if isa(v, 'datetime')
    return
  end
  if (isnumeric(v) || islogical(v)) && ~isempty(v)
    error('Nelson:time:concatenateDatetimeOperands', '%s', 'All inputs must be datetimes or date/time character vectors or date/time strings.');
  end
  v = datetime(v);
end
%=============================================================================
