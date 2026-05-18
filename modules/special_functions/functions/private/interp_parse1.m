%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, v, xq, method, extrapMode, extrapValue, returnPP] = interp_parse1(varargin)
  method = 'linear';
  extrapMode = 'default';
  extrapValue = NaN;
  returnPP = false;
  xq = [];

  args = varargin;
  if numel(args) >= 1 && interp_is_text(args{end})
    last = convertStringsToChars(args{end});
    if strcmp(last, 'pp')
      returnPP = true;
      args(end) = [];
    end
  end

  if returnPP
    if numel(args) < 3
      error(_('Wrong number of input arguments.'));
    end
    x = args{1};
    v = args{2};
    method = interp_method(args{3}, true, true);
    extrapMode = 'extrap';
    return
  end

  if numel(args) >= 1
    tail = args{end};
    if interp_is_text(tail)
      tailText = convertStringsToChars(tail);
      if strcmp(tailText, 'extrap')
        extrapMode = 'extrap';
        args(end) = [];
      end
    elseif isnumeric(tail) && isscalar(tail) && numel(args) >= 4
      extrapMode = 'constant';
      extrapValue = tail;
      args(end) = [];
    end
  end

  if numel(args) >= 1 && interp_is_text(args{end})
    method = interp_method(args{end}, true, true);
    args(end) = [];
  end

  switch numel(args)
    case 2
      v = args{1};
      xq = args{2};
      if isvector(v)
        x = 1:length(v);
      else
        x = 1:size(v, 1);
      end
    case 3
      x = args{1};
      v = args{2};
      xq = args{3};
    otherwise
      error(_('Wrong number of input arguments.'));
  end
end
%=============================================================================
