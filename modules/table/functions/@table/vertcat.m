%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = vertcat(varargin)
  T = varargin{1};
  if ~istable(T)
    error(_('All input arguments must be tables.'));
  end
  
  for k = 2:1:nargin
    T2 = varargin{k};
    if ~istable(T2)
      error(_('All input arguments must be tables.'));
    end
    if isempty(T)
      T = T2;
    elseif ~isempty(T2)
      st1 = struct(T);
      st2 = struct(T2);
      if (~isequal(st1.Properties.VariableNames, st2.Properties.VariableNames))
        error(_('All tables to be vertically concatenated must have identical variable names.'));
      end
      out = struct(T);
      for k = 1:length(st1.Properties.VariableNames)
        variableNames = st1.Properties.VariableNames{k};
        out.data.(variableNames) = [st1.data.(variableNames); st2.data.(variableNames)];
      end
      T = class(out, 'table');
    end
  end
end
%=============================================================================
