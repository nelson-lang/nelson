%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  narginchk(2, 2);
  obj = varargin{1};
  st = struct(obj);
  
  s = varargin{2};
  
  switch (s(1).type)
    case '.'
      extra = {};
      if (numel (s) > 1 && strcmp (s(2).type, '()'))
        if (~isempty (s(2).subs))
          extra = s(2).subs;
        end
        s(2) = [];
      end
      switch (s(1).subs)
        case 'compress'
          varargout{1} = feval('@tdigest/compress',obj);
        case 'percentile'
          varargout{1} = feval('@tdigest/percentile',obj, extra{:});
        case 'quantile'
          varargout{1} = feval('@tdigest/quantile',obj, extra{:});
        case {'compression', 'totalWeight'}
          varargout{1} = st.(s(1).subs);
        otherwise
          msg = sprintf(_("unknown property '%s'."), s(1).subs);
          error(msg);
        end
        return
      otherwise
        msg = _("'%s' indexing is not supported.", s(1).type);
        error (msg);
      end
    end
    %=============================================================================
