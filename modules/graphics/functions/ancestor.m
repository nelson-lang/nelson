%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = ancestor(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  h = varargin{1};
  type = varargin{2};
  
  if (ischar (type))
    type = { type };
  elseif (~iscellstr(type))
    p = [];
  end
  
  do_once = true;
  if (nargin == 3)
    toplevel = varargin{3};
    if (ischar (toplevel) && strcmpi (toplevel, 'toplevel'))
      do_once = false;
    else
      error(_("'toplevel' expected as third argumnent."));
    end
  end
  
  mustReturnEmpty = isempty (h) || ~isgraphics(h);
  p = [];
  if ~mustReturnEmpty 
    p = cell (numel (h), 1);
    H = cell(size(h));
    for i = 1:size(h, 1)
      for j = 1:size(h, 2)
        H{i, j} = h(i, j);
      end
    end
    h = H;
    for nh = 1:numel(h)
      while (true)
        if (isempty(h{nh}))
          break;
        end
        if (any (strcmpi(get(h{nh}, 'Type'), type)))
          p{nh} = h{nh};
          if do_once
            break;
          end
        end
        h{nh} = get(h{nh},  'Parent');
      end
    end
    if (nh == 1)
      p = p{1};
    end
  end
end
%=============================================================================
