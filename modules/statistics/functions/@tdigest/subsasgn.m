%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = subsasgn(obj, s, val)
  
  if (numel (s) > 1)
    error(_('Only one level of indexing is supported.'));
  end
  obj = struct(obj); % Convert to struct for easy field assignment
  
  switch (s(1).type)
    case '.'
      switch (s(1).subs)
        case 'centroids'
          if ~isstruct(val) || ~all(isfield(val, {'mean', 'weight'}))
            error('Centroids must be a struct array with fields ''mean'' and ''weight''.');
          end
          obj.centroids = val;
        case 'compression'
          if ~isscalar(val) || ~isnumeric(val) || val <= 0
            error('Compression must be a positive scalar.');
          end
          obj.compression = val;
        case 'totalWeight'
          if ~isscalar(val) || ~isnumeric(val) || val < 0
            error('Total weight must be a non-negative scalar.');
          end
          obj.totalWeight = val;
        otherwise
          msg = sprintf(_("unknown property '%s'."), s(1).subs);
          error(msg);
        end
      otherwise
        error(_('Only dot indexing is supported.'));
      end
      obj = class(obj, 'tdigest');
    end
    %=============================================================================
