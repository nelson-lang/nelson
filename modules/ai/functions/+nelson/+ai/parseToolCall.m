%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Parse a model reply for a tool call of the form
% {"tool": "<name>", "arguments": {...}}, optionally inside a json code fence.
% Small local models often emit invalid JSON (raw newlines inside the code
% string, trailing commas), so a lenient salvage is attempted before giving up.
function [isTool, name, args] = parseToolCall(reply)
  isTool = false;
  name = '';
  args = struct();
  if nargin < 1 || (~ischar(reply) && ~isstring(reply))
    return
  end
  jsonText = extractJsonBlock(char(reply));
  if isempty(jsonText)
    return
  end
  decoded = strictDecode(jsonText);
  if isempty(decoded)
    [isTool, name, args] = salvageToolCall(jsonText);
    return
  end
  [isTool, name, args] = toolFromDecoded(decoded);
end
%=============================================================================
function [isTool, name, args] = toolFromDecoded(decoded)
  isTool = false;
  name = '';
  args = struct();
  if ~isstruct(decoded) || ~isscalar(decoded) || ~isfield(decoded, 'tool')
    return
  end
  if ~ischar(decoded.tool) && ~isstring(decoded.tool)
    return
  end
  isTool = true;
  name = char(decoded.tool);
  if isfield(decoded, 'arguments') && isstruct(decoded.arguments)
    args = decoded.arguments;
  end
end
%=============================================================================
function decoded = strictDecode(jsonText)
  try
    decoded = jsondecode(jsonText);
  catch
    decoded = [];
  end
end
%=============================================================================
function out = extractJsonBlock(text)
  out = '';
  fenced = regexp(text, '```(?:json)?\s*(\{[\s\S]*\})\s*```', 'tokens', 'once');
  if ~isempty(fenced)
    out = fenced{1};
    return
  end
  firstBrace = strfind(text, '{');
  lastBrace = strfind(text, '}');
  if ~isempty(firstBrace) && ~isempty(lastBrace) && lastBrace(end) > firstBrace(1)
    out = text(firstBrace(1):lastBrace(end));
  end
end
%=============================================================================
% Recover a tool call from malformed JSON: grab the tool name and the first
% string-valued argument. The non-greedy body spans raw newlines, so it also
% covers the common case of unescaped line breaks inside the code string.
function [isTool, name, args] = salvageToolCall(jsonText)
  isTool = false;
  name = '';
  args = struct();
  nameToken = regexp(jsonText, '"tool"\s*:\s*"([^"]+)"', 'tokens', 'once');
  if isempty(nameToken)
    return
  end
  valueToken = regexp(jsonText, ...
    '"(code|command|query|expression|text|input)"\s*:\s*"([\s\S]*?)"\s*\}', 'tokens', 'once');
  if isempty(valueToken)
    return
  end
  value = regexprep(valueToken{2}, '\\n', char(10));
  value = regexprep(value, '\\t', char(9));
  value = regexprep(value, '\\"', '"');
  isTool = true;
  name = nameToken{1};
  args.(valueToken{1}) = value;
end
%=============================================================================
