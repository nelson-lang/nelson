%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = horzcat(varargin)
  T = varargin{1};
  if ~istable(T)
    error(_('All input arguments must be tables.'));
  end
  
  for k = 2:nargin
    T2 = varargin{k};
    if ~istable(T2)
      error(_('All input arguments must be tables.'));
    end
    if isempty(T)
      T = T2;
    elseif ~isempty(T2)
      st1 = struct(T);
      st2 = struct(T2);
      if ~isequal(size(T, 1), size(T2, 1))
        error(_('All tables to be horizontally concatenated must have the same number of rows.'));
      end
      out = struct(T);
      names = [T.Properties.VariableNames, T2.Properties.VariableNames];
      if ~isAllUnique(names)
        error(_('All names must be unique.'));
      end
      out.Properties.VariableNames = names;
      for k = 1:length(st1.Properties.VariableNames)
        out.data.(st1.Properties.VariableNames{k}) = st1.data.(st1.Properties.VariableNames{k});
      end
      for k = 1:length(st2.Properties.VariableNames)
        out.data.(st2.Properties.VariableNames{k}) = st2.data.(st2.Properties.VariableNames{k});
      end
      T = class(out, 'table');
    end
  end
end
%=============================================================================
function tf = isAllUnique(names)
  u = unique(names);
  tf = isequal(size(u), size(names));
end
%=============================================================================
