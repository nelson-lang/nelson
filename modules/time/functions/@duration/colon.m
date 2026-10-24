%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = colon(a, b, c)
  narginchk(2, 3);
  if nargin == 2
    first = a;
    step = seconds(1);
    last = b;
  else
    first = a;
    step = b;
    last = c;
  end
  if ~isa(first, 'duration') || ~isa(step, 'duration') || ~isa(last, 'duration')
    error('Nelson:time:durationColonOperandsExpected', '%s colon operands expected.', 'Duration');
  end
  out = seconds(seconds(first):seconds(step):seconds(last));
  out.Format = first.Format;
end
%=============================================================================
