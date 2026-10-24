%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [m, N] = signalParseWindowLengthAndOption(args)
  m = args{1};
  signalValidateWindowLength(m);
  N = m - 1;
  if length(args) == 2
    opt = args{2};
  else
    opt = 'symmetric';
  end
  if strcmp(opt, 'periodic')
    N = m;
  elseif ~(strcmp(opt, 'symmetric') || strcmp(opt, 'symmetric'))
    error('Nelson:signal:invalidWindowType', '%s expected.', '"periodic" or "symmetric"');
  end
end
%=============================================================================
