%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = replaceBetween(str, start, stop, replacement, varargin)
  narginchk(4, 6);
  nargoutchk(0, 1);
  boundaries = '';
  if nargin >= 6 && strcmpi(char(varargin{1}), 'Boundaries')
    boundaries = char(varargin{2});
  end
  hasMissing = false;
  missingMask = [];
  if isstring(str)
    missingMask = ismissing(str);
    hasMissing = any(missingMask(:));
    str(missingMask) = "";
  end
  [items, ~, kind] = stringPrivateToCellstr(str);
  if isnumeric(start) && isnumeric(stop)
    % Numeric positions: splice the replacement inline. Calling replaceBetweenOne
    % per element routes through a function that references external helpers
    % (regexp, stringPrivatePatternRegex); such a per-element call is
    % pathologically slow in the interpreter, which made this O(numel^2).
    items = replaceBetweenNumeric(items, start, stop, replacement, boundaries, hasMissing, missingMask);
    out = stringPrivateFromCellstr(items, kind);
    if hasMissing
      out(missingMask) = missing;
    end
    return
  end
  for k = 1:numel(items)
    if hasMissing && missingMask(k)
      continue;
    end
    items{k} = replaceBetweenOne(items{k}, selectElement(start, k), selectElement(stop, k), textElement(replacement, k), boundaries);
  end
  out = stringPrivateFromCellstr(items, kind);
  if hasMissing
    out(missingMask) = missing;
  end
end
%=============================================================================
function items = replaceBetweenNumeric(items, start, stop, replacement, boundaries, hasMissing, missingMask)
  exclusive = strcmpi(boundaries, 'exclusive');
  startScalar = isscalar(start);
  stopScalar = isscalar(stop);
  repScalar = ~((isstring(replacement) && numel(replacement) > 1) || (iscell(replacement) && numel(replacement) > 1));
  if repScalar
    repText = textElement(replacement, 1);
  end
  for k = 1:numel(items)
    if hasMissing && missingMask(k)
      continue;
    end
    if startScalar
      s = start;
    else
      s = start(k);
    end
    if stopScalar
      e = stop;
    else
      e = stop(k);
    end
    if exclusive
      s = s + 1;
      e = e - 1;
    end
    if repScalar
      rep = repText;
    else
      rep = textElement(replacement, k);
    end
    txt = items{k};
    items{k} = [txt(1:s - 1), rep, txt(e + 1:end)];
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
function txt = replaceBetweenOne(txt, start, stop, replacement, boundaries)
  if isnumeric(start) && isnumeric(stop)
    s = start;
    e = stop;
    if strcmpi(boundaries, 'exclusive')
      s = s + 1;
      e = e - 1;
    end
    txt = [txt(1:s - 1), replacement, txt(e + 1:end)];
    return
  end
  startExpr = stringPrivatePatternRegex(start);
  stopExpr = stringPrivatePatternRegex(stop);
  inclusive = strcmpi(boundaries, 'inclusive');
  % Find every start/stop match once, then pair them sequentially with
  % monotonic cursors. The former loop re-sliced the tail (rest = rest(e2+1:end))
  % and grew the result string on each match, which is quadratic. Segments are
  % collected into a preallocated cell and joined once.
  [startS, startE] = regexp(txt, startExpr, 'start', 'end');
  [stopS, stopE] = regexp(txt, stopExpr, 'start', 'end');
  nStart = numel(startS);
  nStop = numel(stopS);
  pieces = cell(1, 3 * min(nStart, nStop) + 2);
  pc = 0;
  pos = 1;
  si = 1;
  ti = 1;
  while true
    while si <= nStart && startS(si) < pos
      si = si + 1;
    end
    if si > nStart
      pc = pc + 1;
      pieces{pc} = txt(pos:end);
      break
    end
    s1 = startS(si);
    e1 = startE(si);
    % The stop must begin after the start ends (former rest(e1 + 1:end)).
    while ti <= nStop && stopS(ti) <= e1
      ti = ti + 1;
    end
    if ti > nStop
      pc = pc + 1;
      pieces{pc} = txt(pos:end);
      break
    end
    s2 = stopS(ti);
    e2 = stopE(ti);
    if inclusive
      pc = pc + 1;
      pieces{pc} = txt(pos:s1 - 1);
      pc = pc + 1;
      pieces{pc} = replacement;
    else
      pc = pc + 1;
      pieces{pc} = txt(pos:e1);
      pc = pc + 1;
      pieces{pc} = replacement;
      pc = pc + 1;
      pieces{pc} = txt(s2:e2);
    end
    pos = e2 + 1;
  end
  txt = [pieces{1:pc}];
end
%=============================================================================
