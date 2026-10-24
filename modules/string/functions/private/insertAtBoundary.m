%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = insertAtBoundary(str, position, newText, after)
  hasMissing = false;
  if isstring(str)
    missingMask = ismissing(str);
    hasMissing = any(missingMask(:));
    str(missingMask) = "";
  end
  [items, ~, kind] = stringPrivateToCellstr(str);
  for k = 1:numel(items)
    if hasMissing && missingMask(k)
      continue;
    end
    txt = items{k};
    currentPosition = selectElement(position, k);
    currentText = textElement(newText, k);
    if isnumeric(currentPosition)
      pos = currentPosition;
      if after
        items{k} = [txt(1:pos), currentText, txt(pos + 1:end)];
      else
        items{k} = [txt(1:pos - 1), currentText, txt(pos:end)];
      end
    else
      expr = stringPrivatePatternRegex(currentPosition);
      [s, e] = regexp(txt, expr, 'start', 'end');
      if ~isempty(s)
        % Insert at every match (right-to-left keeps the positions valid).
        result = txt;
        if after
          for j = numel(e):-1:1
            result = [result(1:e(j)), currentText, result(e(j) + 1:end)];
          end
        else
          for j = numel(s):-1:1
            result = [result(1:s(j) - 1), currentText, result(s(j):end)];
          end
        end
        items{k} = result;
      end
    end
  end
  out = stringPrivateFromCellstr(items, kind);
  if hasMissing
    out(missingMask) = missing;
  end
end
%=============================================================================
function txt = textElement(value, k)
  if isstring(value)
    if isscalar(value)
      txt = char(value);
    else
      txt = char(value(k));
    end
  elseif iscell(value)
    if isscalar(value)
      txt = value{1};
    else
      txt = value{k};
    end
  else
    txt = value;
  end
end
%=============================================================================
function value = selectElement(value, k)
  if isnumeric(value) && numel(value) > 1
    value = value(k);
  elseif isstring(value) && numel(value) > 1
    value = value(k);
  elseif iscell(value) && numel(value) > 1
    value = value{k};
  end
end
%=============================================================================
