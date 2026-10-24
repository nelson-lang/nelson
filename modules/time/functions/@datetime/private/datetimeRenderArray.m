%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = datetimeRenderArray(serial, fmt)
  % Vectorized LDML/Unicode renderer for a whole serial-date array. Produces
  % the same per-element text as datetime.formatOne (which calls
  % renderLdmlFormat/renderLdmlToken element by element) but with no
  % per-element formatting or per-element string accumulation, so the cost is
  % linear in the number of elements instead of quadratic. Not-a-time stays
  % the text 'NaT', exactly as formatOne returns it.
  fmt = char(fmt);
  sz = size(serial);
  sv = serial(:);
  N = numel(sv);
  out = repmat("NaT", N, 1);
  if N == 0
    out = reshape(out, sz);
    return
  end
  valid = ~isnan(sv);
  if any(valid)
    try
      out(valid) = renderValidColumn(sv(valid), fmt);
    catch
      % Pathological formats: fall back to the scalar path element by element.
      for k = 1:N
        out(k) = datetime.formatOne(sv(k), fmt);
      end
    end
  end
  out = reshape(out, sz);
end
%=============================================================================
function col = renderValidColumn(sv, fmt)
  % sv holds only finite serials (NaT handled by the caller). Decompose every
  % element at once, then concatenate one vectorized column per LDML token.
  dv = datevec(sv);
  Y = dv(:, 1); Mo = dv(:, 2); D = dv(:, 3);
  H = dv(:, 4); Mi = dv(:, 5);
  sec6 = dv(:, 6);
  Ssec = floor(sec6 + 1e-6);
  ms = round((sec6 - Ssec) * 1000);
  carry = ms >= 1000;
  ms(carry) = ms(carry) - 1000;
  Ssec(carry) = Ssec(carry) + 1;
  wd = weekday(sv);
  wd = wd(:); % 1 = Sunday .. 7 = Saturday
  tokens = tokenizeLdml(fmt);
  col = repmat("", numel(sv), 1);
  for t = 1:numel(tokens)
    tok = tokens(t);
    if tok.field
      col = col + renderFieldColumn(tok.letter, tok.cnt, ...
        Y, Mo, D, H, Mi, Ssec, ms, wd);
    else
      col = col + string(tok.text);
    end
  end
end
%=============================================================================
function tokens = tokenizeLdml(fmt)
  % Split an LDML format into literal runs and letter fields, mirroring the
  % char-by-char scan of datetime.renderLdmlFormat: consecutive identical
  % letters form one field; text inside single quotes is literal ('' escapes a
  % single quote). Driven by the format string only, so its cost never scales
  % with the number of dates.
  tokens = struct('field', {}, 'text', {}, 'letter', {}, 'cnt', {});
  litbuf = '';
  n = numel(fmt);
  i = 1;
  while i <= n
    c = fmt(i);
    if c == ''''
      if i < n && fmt(i + 1) == ''''
        litbuf = [litbuf, ''''];
        i = i + 2;
        continue;
      end
      j = i + 1;
      while j <= n && fmt(j) ~= ''''
        litbuf = [litbuf, fmt(j)];
        j = j + 1;
      end
      i = j + 1;
      continue;
    end
    if isletter(c)
      if ~isempty(litbuf)
        tokens(end + 1) = struct('field', false, 'text', litbuf, 'letter', '', 'cnt', 0);
        litbuf = '';
      end
      j = i;
      while j <= n && fmt(j) == c
        j = j + 1;
      end
      tokens(end + 1) = struct('field', true, 'text', '', 'letter', c, 'cnt', j - i);
      i = j;
    else
      litbuf = [litbuf, c];
      i = i + 1;
    end
  end
  if ~isempty(litbuf)
    tokens(end + 1) = struct('field', false, 'text', litbuf, 'letter', '', 'cnt', 0);
  end
end
%=============================================================================
function s = renderFieldColumn(c, cnt, Y, Mo, D, H, Mi, Ssec, ms, wd)
  % Vectorized counterpart of datetime.renderLdmlToken: each case returns an
  % N x 1 string column. The numeric values fed to compose() are the same ones
  % renderLdmlToken passes to sprintf, so the text is byte-for-byte identical.
  monthFull = {'January', 'February', 'March', 'April', 'May', 'June', ...
    'July', 'August', 'September', 'October', 'November', 'December'};
  monthAbbr = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
  dayFull = {'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', ...
    'Friday', 'Saturday'};
  dayAbbr = {'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'};
  M = numel(Y);
  switch c
    case {'y', 'u'}
      if cnt == 2
        s = compose('%02d', mod(Y, 100));
      else
        s = compose(['%0', int2str(cnt), 'd'], Y);
      end
    case {'M', 'L'}
      if cnt >= 4
        s = reshape(string(monthFull(Mo)), [], 1);
      elseif cnt == 3
        s = reshape(string(monthAbbr(Mo)), [], 1);
      elseif cnt == 2
        s = compose('%02d', Mo);
      else
        s = compose('%d', Mo);
      end
    case 'd'
      if cnt >= 2
        s = compose('%02d', D);
      else
        s = compose('%d', D);
      end
    case {'e', 'E', 'c'}
      if cnt >= 4
        s = reshape(string(dayFull(wd)), [], 1);
      else
        s = reshape(string(dayAbbr(wd)), [], 1);
      end
    case 'H'
      if cnt >= 2
        s = compose('%02d', H);
      else
        s = compose('%d', H);
      end
    case 'h'
      h12 = mod(H, 12);
      h12(h12 == 0) = 12;
      if cnt >= 2
        s = compose('%02d', h12);
      else
        s = compose('%d', h12);
      end
    case 'm'
      if cnt >= 2
        s = compose('%02d', Mi);
      else
        s = compose('%d', Mi);
      end
    case 's'
      if cnt >= 2
        s = compose('%02d', Ssec);
      else
        s = compose('%d', Ssec);
      end
    case 'S'
      base = compose('%03d', ms);
      if cnt <= 3
        s = extractBefore(base, cnt + 1);
      else
        s = base + repmat('0', 1, cnt - 3);
      end
    case 'a'
      s = repmat("AM", M, 1);
      s(H >= 12) = "PM";
    case {'Q', 'q'}
      q = floor((Mo - 1) / 3) + 1;
      if cnt >= 3
        s = "Q" + string(q);
      elseif cnt == 2
        s = compose('%02d', q);
      else
        s = compose('%d', q);
      end
    case 'D'
      doy = floor(datenum(Y, Mo, D)) - floor(datenum(Y, 1, 1)) + 1;
      s = compose(['%0', int2str(cnt), 'd'], doy);
    otherwise
      s = repmat(string(repmat(c, 1, cnt)), M, 1);
  end
end
%=============================================================================
