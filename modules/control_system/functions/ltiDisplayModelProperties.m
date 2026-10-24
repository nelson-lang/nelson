%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ltiDisplayModelProperties(sys, plainFallback)
  if nargin < 2
    plainFallback = '';
  end
  props = ltiPropertyNames(class(sys));
  expandedText = '';
  width = 0;
  for k = 1:numel(props)
    width = max(width, length(props{k}));
  end
  for k = 1:numel(props)
    name = props{k};
    try
      value = sys.(name);
      expandedText = [expandedText, '      ', repmat(' ', 1, width - length(name)), ...
        name, ': ', ltiCompactPropertyValue(value), char(10)];
    catch
    end
  end
  __display_expandable_text__(_('Model Properties'), expandedText, plainFallback);
end
%=============================================================================
function text = ltiCompactPropertyValue(value)
  if isempty(value)
    text = ltiEmptyValueText(value);
  elseif ischar(value)
    if size(value, 1) == 1
      text = ['''', value, ''''];
    else
      text = ltiSizedValueText(value);
    end
  elseif isstring(value)
    if isscalar(value)
      text = ['"', char(value), '"'];
    else
      text = ltiSizedValueText(value);
    end
  elseif isnumeric(value) || islogical(value)
    if isscalar(value) || numel(value) <= 10
      text = mat2str(value, 4);
    else
      text = ltiSizedValueText(value);
    end
  elseif iscell(value)
    text = ltiSizedValueText(value);
  elseif isstruct(value)
    text = ltiSizedValueText(value);
  else
    text = ltiSizedValueText(value);
  end
end
%=============================================================================
function text = ltiEmptyValueText(value)
  if iscell(value)
    text = '{}';
  elseif ischar(value)
    text = '''''';
  else
    text = '[]';
  end
end
%=============================================================================
function text = ltiSizedValueText(value)
  text = ['[', ltiSizeToString(size(value)), ' ', class(value), ']'];
end
%=============================================================================
function text = ltiSizeToString(sz)
  text = int2str(sz(1));
  for k = 2:length(sz)
    text = [text, 'x', int2str(sz(k))];
  end
end
%=============================================================================
