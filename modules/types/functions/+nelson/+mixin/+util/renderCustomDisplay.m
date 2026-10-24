%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Shared renderer for the CustomDisplay mixin. Composes the (possibly
% overridden) header, property groups and footer. All formatting lives here so
% it can evolve without rebuilding the interpreter.
%   h : [] for the default header, or char/string from getHeader
%   g : [] for the default single group, or a PropertyGroup array
%   f : [] for no footer, or char/string from getFooter
%=============================================================================
function renderCustomDisplay(obj, h, g, f)
  % ---- header ----
  if isempty(h)
    disp(['  ', class(obj), ' with properties:']);
  else
    disp(char(h));
  end
  % ---- property groups ----
  if isempty(g)
    printPropertyGroup(obj, '', properties(obj));
  else
    for k = 1:numel(g)
      printPropertyGroup(obj, g(k).Title, g(k).PropertyList);
    end
  end
  % ---- footer ----
  if ~isempty(f)
    disp(char(f));
  end
end
%=============================================================================
function printPropertyGroup(obj, title, names)
  if iscell(names) == false
    names = cellstr(names);
  end
  names = names(:);
  if ~isempty(title)
    fprintf('\n');
    disp(['  ', char(title), ':']);
  end
  fprintf('\n');
  maxLen = 0;
  for k = 1:numel(names)
    maxLen = max(maxLen, numel(names{k}));
  end
  for k = 1:numel(names)
    name = names{k};
    pad = repmat(' ', 1, maxLen - numel(name));
    disp(['    ', pad, name, ': ', summarizeValue(obj.(name))]);
  end
  fprintf('\n');
end
%=============================================================================
function s = summarizeValue(v)
  if ischar(v) && (isrow(v) || isempty(v))
    s = ['''', v, ''''];
  elseif isstring(v) && isscalar(v)
    s = ['"', char(v), '"'];
  elseif (isnumeric(v) || islogical(v)) && isscalar(v)
    s = mat2str(v);
  elseif isempty(v)
    s = '[]';
  else
    dims = sprintf('%dx', size(v));
    dims(end) = [];
    s = ['[', dims, ' ', class(v), ']'];
  end
end
%=============================================================================
