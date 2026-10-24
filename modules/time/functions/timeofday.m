%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = timeofday(t)
  narginchk(1, 1);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  serial = datenum(t);
  d = seconds((serial - floor(serial)) .* 86400);
  d.Format = 'hh:mm:ss';
end
%=============================================================================
