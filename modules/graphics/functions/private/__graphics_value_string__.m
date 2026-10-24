%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Renders a single graphics property value as a short display string for the
% property inspector. Shared by the full model and the light value refresh so
% both backends show identical text. Large or structured values are summarised
% (e.g. '1x100 double') instead of being serialised in full.
%=============================================================================
function text = __graphics_value_string__(raw)
  raw = local_coerce(raw);
  try
    if ischar(raw)
      text = raw;
    elseif isstring(raw) && isscalar(raw)
      text = char(raw);
    elseif islogical(raw) && isscalar(raw)
      text = local_bool_string(raw);
    elseif isnumeric(raw) && isscalar(raw) && isreal(raw)
      text = local_number_string(raw);
    elseif isnumeric(raw) && isreal(raw) && isvector(raw) && numel(raw) <= 8
      parts = cell(1, numel(raw));
      for k = 1:numel(raw)
        parts{k} = local_number_string(raw(k));
      end
      text = ['[', strjoin(parts, ' '), ']'];
    elseif isempty(raw)
      text = '[]';
    else
      text = local_summary(raw);
    end
  catch
    text = local_summary(raw);
  end
end
%=============================================================================
function raw = local_coerce(raw)
  % get() returns on/off switches as an enumeration for which islogical() is
  % true; normalise them to their 'on'/'off' string so display and JSON
  % encoding stay on plain text.
  if strcmp(class(raw), 'nelson.lang.OnOffSwitchState')
    raw = char(raw);
  end
end
%=============================================================================
function text = local_summary(raw)
  dims = size(raw);
  text = sprintf('%d', dims(1));
  for k = 2:numel(dims)
    text = [text, char(215), sprintf('%d', dims(k))];
  end
  text = [text, ' ', class(raw)];
end
%=============================================================================
function text = local_number_string(value)
  if ~isfinite(value)
    text = num2str(value);
    return;
  end
  if abs(value) < 1e-12
    value = 0;
  end
  if value == floor(value) && abs(value) < 1e15
    text = sprintf('%.0f', value);
  else
    text = sprintf('%.4f', value);
  end
end
%=============================================================================
function text = local_bool_string(value)
  if value
    text = 'true';
  else
    text = 'false';
  end
end
%=============================================================================
