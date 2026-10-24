%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = extractBetween(str, start, stop, varargin)
  narginchk(3, 5);
  nargoutchk(0, 1);
  boundaries = '';
  if nargin >= 5 && strcmpi(char(varargin{1}), 'Boundaries')
    boundaries = char(varargin{2});
  end
  hasMissing = false;
  if isstring(str)
    missingMask = ismissing(str);
    hasMissing = any(missingMask(:));
    str(missingMask) = "";
  end
  [items, ~, kind] = stringPrivateToCellstr(str);
  if isnumeric(start) && isnumeric(stop)
    % Numeric positions: extract the substring inline. Calling extractBetweenOne
    % per element routes through a function that references external helpers
    % (regexp, stringPrivatePatternRegex); such a per-element call is
    % pathologically slow in the interpreter, which made this O(numel^2).
    out = extractBetweenNumeric(items, kind, start, stop, boundaries);
    if hasMissing
      if isscalar(str)
        out = string(missing);
      else
        out(missingMask) = missing;
      end
    end
    return
  end
  result = cell(size(items));
  for k = 1:numel(items)
    extracted = extractBetweenOne(items{k}, selectElement(start, k), selectElement(stop, k), boundaries);
    if iscell(extracted)
      if numel(items) == 1
        result = extracted(:);
        break
      elseif isempty(extracted)
        result{k} = '';
      else
        result{k} = extracted{1};
      end
    else
      result{k} = extracted;
    end
  end
  out = stringPrivateFromCellstr(result, kind, strcmp(kind, 'char'));
  if hasMissing
    if isscalar(str)
      out = string(missing);
    else
      out(missingMask) = missing;
    end
  end
end
%=============================================================================
function out = extractBetweenNumeric(items, kind, start, stop, boundaries)
  exclusive = strcmpi(boundaries, 'exclusive');
  startScalar = isscalar(start);
  stopScalar = isscalar(stop);
  result = cell(size(items));
  for k = 1:numel(items)
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
    txt = items{k};
    result{k} = txt(max(1, s):min(length(txt), e));
  end
  out = stringPrivateFromCellstr(result, kind, strcmp(kind, 'char'));
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
function value = extractBetweenOne(txt, start, stop, boundaries)
  if isnumeric(start) && isnumeric(stop)
    s = start;
    e = stop;
    if strcmpi(boundaries, 'exclusive')
      s = s + 1;
      e = e - 1;
    end
    value = txt(max(1, s):min(length(txt), e));
    return
  end
  startExpr = stringPrivatePatternRegex(start);
  stopExpr = stringPrivatePatternRegex(stop);
  inclusive = strcmpi(boundaries, 'inclusive');
  % Find every start/stop match once, then pair them sequentially with
  % monotonic cursors. The former loop re-sliced the tail (txt(offset:end)) and
  % grew the result cell on each match, which is quadratic.
  [startS, startE] = regexp(txt, startExpr, 'start', 'end');
  [stopS, stopE] = regexp(txt, stopExpr, 'start', 'end');
  nStart = numel(startS);
  nStop = numel(stopS);
  value = cell(min(nStart, nStop), 1);
  vc = 0;
  pos = 1;
  si = 1;
  ti = 1;
  while true
    while si <= nStart && startS(si) < pos
      si = si + 1;
    end
    if si > nStart
      break
    end
    s1 = startS(si);
    e1 = startE(si);
    % The stop must begin after the start ends (former txt(e1 + 1:end)).
    while ti <= nStop && stopS(ti) <= e1
      ti = ti + 1;
    end
    if ti > nStop
      break
    end
    s2 = stopS(ti);
    e2 = stopE(ti);
    vc = vc + 1;
    if inclusive
      value{vc} = txt(s1:e2);
    else
      value{vc} = txt(e1 + 1:s2 - 1);
    end
    pos = e2 + 1;
  end
  value = value(1:vc);
end
%=============================================================================
