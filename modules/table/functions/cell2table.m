%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cell2table(varargin)
  ce  = varargin{1};    
  if (nargin > 1 && (strcmpi(varargin{2}, 'VariableNames')))
    if isrow(ce)
      varargout{1} = table(ce{:}, 'VariableNames', varargin{3});
    else
      varargout{1} = table(ce, 'VariableNames', varargin{3});
    end
  else
    prefix = inputname(1);
    if (~isempty(prefix) && ~isempty(varargin{1}))
      cell_size = size(varargin{1});
      names = {};
      for k=1:cell_size(2)
        names{k} = [prefix, num2str(k)]; 
      end
      varargout{1} = table(varargin{1}, 'VariableNames', names);
    else
      varargout{1} = table(varargin{1});
    end
  end
end
%=============================================================================
