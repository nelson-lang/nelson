%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [args, method, extrapMode, extrapValue] = interp_parse_tail(args, allowPchip)
  method = 'linear';
  extrapMode = 'default';
  extrapValue = NaN;

  if numel(args) >= 2
    tail = args{end};
    if isnumeric(tail) && isscalar(tail) && interp_is_text(args{end - 1})
      extrapMode = 'constant';
      extrapValue = tail;
      args(end) = [];
    end
  end

  if numel(args) >= 1 && interp_is_text(args{end})
    method = interp_method(args{end}, allowPchip, false);
    args(end) = [];
  end
end
%=============================================================================
