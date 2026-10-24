%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, position] = textscan(varargin)
  % Read formatted data from a character vector, string or open file.
  % C = textscan(chr, format)
  % C = textscan(fid, format)
  % C = textscan(..., Name, Value)
  % see also sscanf, fscanf, fopen, fread.
  narginchk(1, Inf);
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  src = varargin{1};
  fmt = varargin{2};
  if isstring(fmt) && isscalar(fmt)
    fmt = char(fmt);
  end
  if ~ischar(fmt) || (~isempty(fmt) && size(fmt, 1) ~= 1)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Format');
  end
  s = readSource(src);

  opts = parseTextscanOptions(varargin(3:end));

  s = applyHeaderLines(s, opts.headerLines);
  s = applyCommentStyle(s, opts.commentStyle);

  specs = parseTextscanFormat(fmt);
  ncol = 0;
  for k = 1:numel(specs)
    if strcmp(specs{k}.kind, 'conv') && ~specs{k}.skip
      ncol = ncol + 1;
    end
  end
  if ncol == 0
    error('Nelson:textscan:noConversion', 'The format must contain at least one conversion specifier.');
  end

  [handled, C, position] = tryFastNumeric(s, specs, ncol, opts);
  if ~handled
    [raw, colClass, position] = scanTextscan(s, specs, ncol, opts);
    C = cell(1, ncol);
    for k = 1:ncol
      C{k} = buildColumn(raw{k}, colClass{k});
    end
  end

  if opts.collectOutput
    C = collectColumns(C);
  end
end
%=============================================================================
function [handled, C, position] = tryFastNumeric(s, specs, ncol, opts)
  % Fast path: when the format is only numeric conversions and literals, with
  % no delimiter, no custom whitespace and no TreatAsEmpty, the whole parse can
  % be delegated to sscanf (a compiled builtin). Numbers are read as double and
  % then cast to each column class, which reproduces the rounding of the
  % integer conversion specifiers.
  handled = false;
  C = {};
  position = 0;
  if ~isempty(opts.delims) || opts.userWhitespace || ~isempty(opts.treatAsEmpty)
    return;
  end
  sscanFmt = '';
  colClass = cell(1, ncol);
  for k = 1:numel(specs)
    sp = specs{k};
    if strcmp(sp.kind, 'lit')
      if any(sp.text == '%')
        return;
      end
      sscanFmt = [sscanFmt, sp.text];
    else
      if sp.skip || sp.hasWidth || ~any(sp.conv == 'duf')
        return;
      end
      sscanFmt = [sscanFmt, '%f'];
      colClass{sp.col} = sp.class;
    end
  end
  [v, ~, ~, nextindex] = sscanf(s, sscanFmt);
  v = v(:);
  total = numel(v);
  C = cell(1, ncol);
  full = floor(total / ncol);
  remcols = total - full * ncol;
  if full > 0
    M = reshape(v(1:(full * ncol)), ncol, full).';
  else
    M = zeros(0, ncol);
  end
  for k = 1:ncol
    colv = M(:, k);
    if k <= remcols
      colv = [colv; v(full * ncol + k)];
    end
    C{k} = castNumeric(colv, colClass{k});
  end
  position = nextindex - 1;
  handled = true;
end
%=============================================================================
function s = readSource(src)
  if isstring(src) && isscalar(src)
    s = char(src);
  elseif ischar(src)
    if isempty(src)
      s = '';
    else
      s = reshape(src.', 1, []);
    end
  elseif isnumeric(src) && isscalar(src)
    fid = double(src);
    data = fread(fid, '*char');
    s = reshape(data, 1, []);
  else
    error('Nelson:textscan:invalidSource', 'First argument must be a character vector, a string or a file identifier.');
  end
end
%=============================================================================
function opts = parseTextscanOptions(args)
  opts.delims = {};
  opts.headerLines = 0;
  opts.collectOutput = false;
  opts.emptyValue = NaN;
  opts.multiDelims = false;
  opts.commentStyle = {};
  opts.treatAsEmpty = {};
  ws = [' ', char(9), char(8)];
  userWhitespace = false;
  if mod(numel(args), 2) ~= 0
    error('Nelson:textscan:nameValuePairs', 'Name-value arguments must come in pairs.');
  end
  for k = 1:2:numel(args)
    name = args{k};
    value = args{k + 1};
    if isstring(name) && isscalar(name)
      name = char(name);
    end
    if ~ischar(name)
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Option name');
    end
    switch lower(name)
      case 'delimiter'
        opts.delims = toStringList(value);
      case 'headerlines'
        opts.headerLines = double(value);
      case 'collectoutput'
        opts.collectOutput = toLogical(value);
      case 'emptyvalue'
        opts.emptyValue = double(value);
      case 'multipledelimsasone'
        opts.multiDelims = toLogical(value);
      case 'whitespace'
        ws = toChar(value);
        userWhitespace = true;
      case 'commentstyle'
        opts.commentStyle = toStringList(value);
      case 'treatasempty'
        opts.treatAsEmpty = toStringList(value);
      case 'endofline'
        % End of line characters are always treated as whitespace separators
        % in this implementation; the option is accepted but has no additional
        % effect beyond the default line handling.
      otherwise
        error('Nelson:textscan:unknownOption', 'Unknown option ''%s''.', name);
    end
  end
  if userWhitespace
    ws = [ws, char(10), char(13)];
  else
    ws = [ws, char(10), char(13)];
  end
  wsMap = false(1, 256);
  codes = double(ws);
  codes = codes(codes >= 0 & codes < 256);
  wsMap(codes + 1) = true;
  opts.wsMap = wsMap;
  opts.userWhitespace = userWhitespace;
end
%=============================================================================
function s = applyHeaderLines(s, n)
  if n <= 0 || isempty(s)
    return;
  end
  lines = splitLines(s);
  if n >= numel(lines)
    s = '';
    return;
  end
  s = joinLines(lines((n + 1):end));
end
%=============================================================================
function s = applyCommentStyle(s, style)
  if isempty(style) || isempty(s)
    return;
  end
  if numel(style) >= 2
    startTok = style{1};
    endTok = style{2};
    s = removeBlockComments(s, startTok, endTok);
    return;
  end
  tok = style{1};
  if isempty(tok)
    return;
  end
  lines = splitLines(s);
  for k = 1:numel(lines)
    idx = strfind(lines{k}, tok);
    if ~isempty(idx)
      lines{k} = lines{k}(1:(idx(1) - 1));
    end
  end
  s = joinLines(lines);
end
%=============================================================================
function specs = parseTextscanFormat(fmt)
  specs = {};
  i = 1;
  n = length(fmt);
  col = 0;
  while i <= n
    if fmt(i) == '%'
      i = i + 1;
      if i <= n && fmt(i) == '%'
        i = i + 1;
        spp.kind = 'lit';
        spp.text = '%';
        specs{end + 1} = spp;
        clear spp;
        continue;
      end
      sp.kind = 'conv';
      sp.skip = false;
      if i <= n && fmt(i) == '*'
        sp.skip = true;
        i = i + 1;
      end
      w = 0;
      hasW = false;
      while i <= n && fmt(i) >= '0' && fmt(i) <= '9'
        w = w * 10 + (fmt(i) - '0');
        i = i + 1;
        hasW = true;
      end
      sp.width = w;
      sp.hasWidth = hasW;
      sp.set = '';
      sp.setneg = false;
      if i > n
        error('Nelson:textscan:badFormat', 'Invalid format specifier.');
      end
      c = fmt(i);
      if c == '['
        i = i + 1;
        neg = false;
        if i <= n && fmt(i) == '^'
          neg = true;
          i = i + 1;
        end
        setStart = i;
        if i <= n && fmt(i) == ']'
          i = i + 1;
        end
        while i <= n && fmt(i) ~= ']'
          i = i + 1;
        end
        sp.set = fmt(setStart:(i - 1));
        sp.setneg = neg;
        if i <= n
          i = i + 1;
        end
        sp.conv = 'r';
        sp.class = 'cellstr';
      elseif c == 'd' || c == 'u'
        i = i + 1;
        bits = 0;
        while i <= n && fmt(i) >= '0' && fmt(i) <= '9'
          bits = bits * 10 + (fmt(i) - '0');
          i = i + 1;
        end
        sp.conv = c;
        sp.class = pickIntClass(bits, c == 'd');
      elseif c == 'f' || c == 'g' || c == 'e'
        i = i + 1;
        bits = 0;
        while i <= n && fmt(i) >= '0' && fmt(i) <= '9'
          bits = bits * 10 + (fmt(i) - '0');
          i = i + 1;
        end
        sp.conv = 'f';
        if bits == 32
          sp.class = 'single';
        else
          sp.class = 'double';
        end
      elseif c == 's'
        i = i + 1;
        sp.conv = 's';
        sp.class = 'cellstr';
      elseif c == 'q'
        i = i + 1;
        sp.conv = 'q';
        sp.class = 'cellstr';
      elseif c == 'c'
        i = i + 1;
        sp.conv = 'c';
        sp.class = 'char';
      else
        error('Nelson:textscan:unsupportedSpec', 'Unsupported conversion specifier: %%%s', c);
      end
      if sp.skip
        sp.col = 0;
      else
        col = col + 1;
        sp.col = col;
      end
      specs{end + 1} = sp;
      clear sp;
    else
      litStart = i;
      while i <= n && fmt(i) ~= '%'
        i = i + 1;
      end
      sp2.kind = 'lit';
      sp2.text = fmt(litStart:(i - 1));
      specs{end + 1} = sp2;
      clear sp2;
    end
  end
end
%=============================================================================
function [raw, colClass, position] = scanTextscan(s, specs, ncol, opts)
  n = length(s);
  wsMap = opts.wsMap;
  delims = opts.delims;
  delimMode = ~isempty(delims);
  multi = opts.multiDelims;
  emptyValue = opts.emptyValue;
  treatAsEmpty = opts.treatAsEmpty;
  hasTreatAsEmpty = ~isempty(treatAsEmpty);
  nSpecs = numel(specs);

  colClass = cell(1, ncol);
  for k = 1:nSpecs
    sp = specs{k};
    if strcmp(sp.kind, 'conv') && sp.col > 0
      colClass{sp.col} = sp.class;
    end
  end

  % Values are pushed into a single flat buffer together with the column they
  % belong to. A single level cell is assigned in place, which avoids the
  % quadratic cost of growing a nested per-column cell array.
  flatCap = 256;
  flat = cell(1, flatCap);
  flatCol = zeros(1, flatCap);
  fcnt = 0;

  pos = 1;
  needDelim = false;
  stuck = 0;
  running = true;
  while running
    if pos > n
      break;
    end
    cycleStart = pos;
    for idx = 1:nSpecs
      sp = specs{idx};
      if strcmp(sp.kind, 'lit')
        [pos, matched] = matchLiteral(s, pos, sp.text, wsMap, n);
        if ~matched
          running = false;
          break;
        end
        if hasNonWsChar(sp.text, wsMap)
          needDelim = false;
        end
      else
        if needDelim && delimMode
          pos = consumeDelims(s, pos, wsMap, delims, multi, n);
          needDelim = false;
        end
        storeCol = 0;
        switch sp.conv
          case {'d', 'u', 'f'}
            p2 = skipWs(s, pos, wsMap, n);
            emptyToken = false;
            if hasTreatAsEmpty
              [tok, p3] = grabPlainField(s, p2, wsMap, delims, delimMode, n);
              if ~isempty(tok) && isTreatAsEmpty(tok, treatAsEmpty)
                tok = '';
                emptyToken = true;
              end
            else
              tok = '';
            end
            if emptyToken
              ok = false;
            else
              [numValue, numNewpos, ok] = parseNumberToken(s, p2, n);
            end
            if ok
              pos = numNewpos;
              storeCol = sp.col;
              storeVal = numValue;
              needDelim = true;
            else
              if delimMode && (emptyToken || isDelimAt(s, p2, delims, n))
                storeCol = sp.col;
                storeVal = emptyValue;
                pos = p2;
                needDelim = true;
              else
                pos = p2;
                running = false;
                break;
              end
            end
          case {'s', 'q'}
            p2 = skipWs(s, pos, wsMap, n);
            if sp.conv == 'q'
              [tok, p3, gotQuote] = readQuoted(s, p2, wsMap, delims, delimMode, sp, n);
            else
              gotQuote = false;
              [tok, p3] = readString(s, p2, wsMap, delims, delimMode, sp, n);
            end
            if ~isempty(tok) || gotQuote || (delimMode && isDelimAt(s, p2, delims, n))
              pos = p3;
              storeCol = sp.col;
              storeVal = tok;
              needDelim = true;
            else
              pos = p2;
              running = false;
              break;
            end
          case 'r'
            [tok, p3] = readScanset(s, pos, sp, n);
            if ~isempty(tok)
              pos = p3;
              storeCol = sp.col;
              storeVal = tok;
              needDelim = true;
            else
              running = false;
              break;
            end
          case 'c'
            [tok, p3] = readChars(s, pos, sp, n);
            if ~isempty(tok)
              pos = p3;
              storeCol = sp.col;
              storeVal = tok;
              needDelim = true;
            else
              running = false;
              break;
            end
          otherwise
            running = false;
            break;
        end
        if storeCol > 0
          fcnt = fcnt + 1;
          if fcnt > flatCap
            flat = [flat, cell(1, flatCap)];
            flatCol = [flatCol, zeros(1, flatCap)];
            flatCap = 2 * flatCap;
          end
          flat{fcnt} = storeVal;
          flatCol(fcnt) = storeCol;
        end
      end
    end
    if pos == cycleStart
      stuck = stuck + 1;
      if stuck >= 2
        break;
      end
    else
      stuck = 0;
    end
  end

  raw = cell(1, ncol);
  if fcnt > 0
    flat = flat(1:fcnt);
    colIdx = flatCol(1:fcnt);
    for k = 1:ncol
      raw{k} = flat(colIdx == k);
    end
  else
    for k = 1:ncol
      raw{k} = {};
    end
  end
  position = pos - 1;
end
%=============================================================================
function pos = skipWs(s, pos, wsMap, n)
  while pos <= n && isWsChar(s(pos), wsMap)
    pos = pos + 1;
  end
end
%=============================================================================
function tf = hasNonWsChar(text, wsMap)
  tf = false;
  for k = 1:length(text)
    if ~isWsChar(text(k), wsMap)
      tf = true;
      return;
    end
  end
end
%=============================================================================
function tf = isWsChar(ch, wsMap)
  d = double(ch);
  if d >= 0 && d < 256
    tf = wsMap(d + 1);
  else
    tf = false;
  end
end
%=============================================================================
function [pos, matched] = matchLiteral(s, pos, text, wsMap, n)
  ti = 1;
  tn = length(text);
  matched = true;
  while ti <= tn
    tc = text(ti);
    if isWsChar(tc, wsMap)
      pos = skipWs(s, pos, wsMap, n);
      while ti <= tn && isWsChar(text(ti), wsMap)
        ti = ti + 1;
      end
    else
      if pos <= n && s(pos) == tc
        pos = pos + 1;
        ti = ti + 1;
      else
        matched = false;
        return;
      end
    end
  end
end
%=============================================================================
function pos = consumeDelims(s, pos, wsMap, delims, multi, n)
  pos = skipWs(s, pos, wsMap, n);
  [pos, m] = matchOneDelim(s, pos, delims, n);
  if m && multi
    while true
      p1 = skipWs(s, pos, wsMap, n);
      [p2, m2] = matchOneDelim(s, p1, delims, n);
      if m2
        pos = p2;
      else
        break;
      end
    end
  end
  pos = skipWs(s, pos, wsMap, n);
end
%=============================================================================
function [pos, matched] = matchOneDelim(s, pos, delims, n)
  matched = false;
  for k = 1:numel(delims)
    d = delims{k};
    dl = length(d);
    if dl > 0 && (pos + dl - 1) <= n && strcmp(s(pos:(pos + dl - 1)), d)
      pos = pos + dl;
      matched = true;
      return;
    end
  end
end
%=============================================================================
function tf = isDelimAt(s, pos, delims, n)
  tf = false;
  for k = 1:numel(delims)
    d = delims{k};
    dl = length(d);
    if dl > 0 && (pos + dl - 1) <= n && strcmp(s(pos:(pos + dl - 1)), d)
      tf = true;
      return;
    end
  end
end
%=============================================================================
function [tok, pos] = grabPlainField(s, pos, wsMap, delims, delimMode, n)
  start = pos;
  while pos <= n
    ch = s(pos);
    if isWsChar(ch, wsMap)
      break;
    end
    if delimMode && isDelimAt(s, pos, delims, n)
      break;
    end
    pos = pos + 1;
  end
  tok = s(start:(pos - 1));
end
%=============================================================================
function [tok, pos] = readString(s, pos, wsMap, delims, delimMode, sp, n)
  start = pos;
  count = 0;
  while pos <= n
    if sp.hasWidth && count >= sp.width
      break;
    end
    ch = s(pos);
    if isWsChar(ch, wsMap)
      break;
    end
    if delimMode && isDelimAt(s, pos, delims, n)
      break;
    end
    pos = pos + 1;
    count = count + 1;
  end
  tok = s(start:(pos - 1));
end
%=============================================================================
function [tok, pos, gotQuote] = readQuoted(s, pos, wsMap, delims, delimMode, sp, n)
  gotQuote = false;
  if pos <= n && s(pos) == '"'
    gotQuote = true;
    pos = pos + 1;
    buf = '';
    while pos <= n
      ch = s(pos);
      if ch == '"'
        if (pos + 1) <= n && s(pos + 1) == '"'
          buf = [buf, '"'];
          pos = pos + 2;
        else
          pos = pos + 1;
          break;
        end
      else
        buf = [buf, ch];
        pos = pos + 1;
      end
    end
    tok = buf;
  else
    [tok, pos] = readString(s, pos, wsMap, delims, delimMode, sp, n);
  end
end
%=============================================================================
function [tok, pos] = readScanset(s, pos, sp, n)
  % The character set is matched literally: a '-' is an ordinary set member
  % and does not introduce a range.
  expanded = sp.set;
  neg = sp.setneg;
  start = pos;
  count = 0;
  while pos <= n
    if sp.hasWidth && count >= sp.width
      break;
    end
    ch = s(pos);
    inSet = any(ch == expanded);
    if neg
      inSet = ~inSet;
    end
    if ~inSet
      break;
    end
    pos = pos + 1;
    count = count + 1;
  end
  tok = s(start:(pos - 1));
end
%=============================================================================
function [tok, pos] = readChars(s, pos, sp, n)
  if sp.hasWidth
    w = sp.width;
  else
    w = 1;
  end
  avail = n - pos + 1;
  cnt = min(w, avail);
  if cnt <= 0
    tok = '';
    return;
  end
  tok = s(pos:(pos + cnt - 1));
  pos = pos + cnt;
end
%=============================================================================
function [value, newpos, ok] = parseNumberToken(s, p2, n)
  value = 0;
  newpos = p2;
  ok = false;
  j = p2;
  if j > n
    return;
  end
  start = j;
  if s(j) == '+' || s(j) == '-'
    j = j + 1;
  end
  matchedSpecial = false;
  if (j + 2) <= n && strcmpi(s(j:(j + 2)), 'inf')
    j = j + 3;
    matchedSpecial = true;
  elseif (j + 2) <= n && strcmpi(s(j:(j + 2)), 'nan')
    j = j + 3;
    matchedSpecial = true;
  end
  if ~matchedSpecial
    haveInt = false;
    while j <= n && s(j) >= '0' && s(j) <= '9'
      j = j + 1;
      haveInt = true;
    end
    haveFrac = false;
    if j <= n && s(j) == '.'
      j = j + 1;
      while j <= n && s(j) >= '0' && s(j) <= '9'
        j = j + 1;
        haveFrac = true;
      end
    end
    if ~haveInt && ~haveFrac
      return;
    end
    if j <= n && (s(j) == 'e' || s(j) == 'E')
      k = j + 1;
      if k <= n && (s(k) == '+' || s(k) == '-')
        k = k + 1;
      end
      if k <= n && s(k) >= '0' && s(k) <= '9'
        k = k + 1;
        while k <= n && s(k) >= '0' && s(k) <= '9'
          k = k + 1;
        end
        j = k;
      end
    end
  end
  numStr = s(start:(j - 1));
  d = str2double(numStr);
  if isnan(d) && ~matchedSpecial
    return;
  end
  value = d;
  newpos = j;
  ok = true;
end
%=============================================================================
function tf = isTreatAsEmpty(tok, treatAsEmpty)
  tf = false;
  for k = 1:numel(treatAsEmpty)
    if strcmp(tok, treatAsEmpty{k})
      tf = true;
      return;
    end
  end
end
%=============================================================================
function v = buildColumn(rawCol, cls)
  switch cls
    case 'cellstr'
      v = reshape(rawCol, [], 1);
    case 'char'
      if isempty(rawCol)
        v = '';
      else
        v = char(rawCol{:});
      end
    otherwise
      if isempty(rawCol)
        d = zeros(0, 1);
      else
        d = zeros(numel(rawCol), 1);
        for k = 1:numel(rawCol)
          d(k) = rawCol{k};
        end
      end
      v = castNumeric(d, cls);
  end
end
%=============================================================================
function v = castNumeric(d, cls)
  switch cls
    case 'double'
      v = d;
    case 'single'
      v = single(d);
    case 'int8'
      v = int8(d);
    case 'int16'
      v = int16(d);
    case 'int32'
      v = int32(d);
    case 'int64'
      v = int64(d);
    case 'uint8'
      v = uint8(d);
    case 'uint16'
      v = uint16(d);
    case 'uint32'
      v = uint32(d);
    case 'uint64'
      v = uint64(d);
    otherwise
      v = d;
  end
end
%=============================================================================
function C = collectColumns(C)
  out = {};
  k = 1;
  ncol = numel(C);
  while k <= ncol
    group = {C{k}};
    cls = class(C{k});
    j = k + 1;
    while j <= ncol && strcmp(class(C{j}), cls)
      group{end + 1} = C{j};
      j = j + 1;
    end
    if numel(group) == 1
      out{end + 1} = group{1};
    else
      out{end + 1} = horzcat(group{:});
    end
    k = j;
  end
  C = out;
end
%=============================================================================
function cls = pickIntClass(bits, signedType)
  switch bits
    case 8
      base = 'int8';
    case 16
      base = 'int16';
    case 64
      base = 'int64';
    otherwise
      base = 'int32';
  end
  if signedType
    cls = base;
  else
    cls = ['u', base];
  end
end
%=============================================================================
function lst = toStringList(value)
  if isempty(value)
    lst = {};
  elseif ischar(value)
    lst = {value};
  elseif isstring(value)
    lst = cellstr(value);
    lst = reshape(lst, 1, []);
  elseif iscell(value)
    lst = reshape(value, 1, []);
    for k = 1:numel(lst)
      if isstring(lst{k})
        lst{k} = char(lst{k});
      end
    end
  else
    error('Nelson:textscan:expectedTextOrCell', 'Expected a character vector, string or cell of character vectors.');
  end
end
%=============================================================================
function c = toChar(value)
  if isstring(value) && isscalar(value)
    c = char(value);
  elseif ischar(value)
    c = value;
  else
    error('Nelson:textscan:invalidOptionValue', 'Expected a character vector.');
  end
end
%=============================================================================
function tf = toLogical(value)
  if islogical(value) && isscalar(value)
    tf = value;
  elseif isnumeric(value) && isscalar(value)
    tf = value ~= 0;
  else
    error('Nelson:textscan:expectedLogicalScalar', 'Expected a logical scalar.');
  end
end
%=============================================================================
function lines = splitLines(s)
  s = strrep(s, char(13), '');
  parts = strsplit(s, char(10), 'CollapseDelimiters', false);
  lines = reshape(parts, 1, []);
end
%=============================================================================
function s = joinLines(lines)
  s = strjoin(lines, char(10));
end
%=============================================================================
function s = removeBlockComments(s, startTok, endTok)
  while true
    i1 = strfind(s, startTok);
    if isempty(i1)
      break;
    end
    i1 = i1(1);
    rest = s((i1 + length(startTok)):end);
    i2 = strfind(rest, endTok);
    if isempty(i2)
      s = s(1:(i1 - 1));
      break;
    end
    i2 = i2(1);
    tailStart = i1 + length(startTok) + i2 - 1 + length(endTok);
    s = [s(1:(i1 - 1)), s(tailStart:end)];
  end
end
%=============================================================================
