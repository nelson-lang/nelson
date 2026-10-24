%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = durationRenderArray(values, fmt)
  % Vectorized duration text renderer. Produces the same per-element text as
  % duration.formatOne (unit formats y/d/h/m/s, or a clock format) applied
  % element by element, but in one pass: no per-element formatting and no
  % per-element string accumulation, so the cost is linear. NaN stays the
  % text 'NaN', as formatOne returns it.
  sz = size(values);
  v = values(:);
  switch lower(char(fmt))
    case 'y'
      out = unitColumn(v ./ (86400 * 365.2425), 'yr', 'yrs');
    case 'd'
      out = unitColumn(v ./ 86400, 'day', 'days');
    case 'h'
      out = unitColumn(v ./ 3600, 'hr', 'hr');
    case 'm'
      out = unitColumn(v ./ 60, 'min', 'min');
    case 's'
      out = unitColumn(v, 'sec', 'sec');
    otherwise
      out = clockColumn(v, char(fmt));
  end
  out = reshape(out, sz);
end
%=============================================================================
function out = unitColumn(scaled, singularUnit, pluralUnit)
  % Mirrors duration.formatWithUnit: the five-significant-digit number text
  % plus a unit that is singular only when the scaled magnitude is exactly 1.
  num = shortNumberTextColumn(scaled);
  unit = repmat(string(pluralUnit), size(scaled));
  unit(abs(scaled) == 1) = string(singularUnit);
  out = num + " " + unit;
end
%=============================================================================
function out = shortNumberTextColumn(v)
  % Vectorized duration.shortNumberText. Zero and non-finite values use '%g';
  % finite values use five significant digits, the notation chosen from the
  % exponent of the unrounded value. The per-element decimal precision 4-e is
  % constant within an exponent group, so each group is one compose() call.
  v = v(:);
  out = strings(numel(v), 1);
  if isempty(v)
    return
  end
  special = (v == 0) | ~isfinite(v);
  if any(special)
    out(special) = compose('%g', v(special) + 0);
  end
  finite = ~special;
  if ~any(finite)
    return
  end
  vf = v(finite);
  e = floor(log10(abs(vf)));
  res = strings(numel(vf), 1);
  sci = (e < -4) | (e >= 5);
  if any(sci)
    txt = compose('%.4e', vf(sci));
    res(sci) = regexprep(txt, '\.?0+e', 'e');
  end
  fixed = ~sci;
  if any(fixed)
    eFixed = e(fixed);
    vFixed = vf(fixed);
    resFixed = strings(numel(vFixed), 1);
    groups = unique(eFixed);
    for g = 1:numel(groups)
      sel = (eFixed == groups(g));
      precision = 4 - groups(g);
      txt = compose(sprintf('%%.%df', precision), vFixed(sel));
      if precision > 0
        txt = regexprep(txt, '\.?0+$', '');
      end
      resFixed(sel) = txt;
    end
    res(fixed) = resFixed;
  end
  out(finite) = res;
end
%=============================================================================
function out = clockColumn(v, fmt)
  % Mirrors duration.formatClock for the whole array: NaN/Inf keep their text,
  % finite values are split into clock fields once and formatted per branch.
  lf = lower(fmt);
  out = strings(numel(v), 1);
  nanMask = isnan(v);
  infMask = isinf(v);
  out(nanMask) = "NaN";
  out(infMask & v > 0) = "Inf";
  out(infMask & v < 0) = "-Inf";
  finite = ~(nanMask | infMask);
  if ~any(finite)
    return
  end
  vf = v(finite);
  signText = strings(numel(vf), 1);
  signText(vf < 0) = "-";
  a = abs(vf);
  totalSeconds = floor(a);
  fraction = a - totalSeconds;
  h = floor(totalSeconds / 3600);
  r = totalSeconds - h * 3600;
  mi = floor(r / 60);
  s = r - mi * 60;
  if startsWith(lf, 'dd:')
    d = floor(h / 24);
    hh = h - d * 24;
    body = compose('%02d:%02d:%02d:%02d', [d, hh, mi, s]);
  elseif strcmp(lf, 'hh:mm')
    hh = floor(totalSeconds / 3600);
    mm = floor((totalSeconds - hh * 3600) / 60);
    body = compose('%02d:%02d', [hh, mm]);
  elseif startsWith(lf, 'mm:')
    totalMinutes = floor(totalSeconds / 60);
    sr = totalSeconds - totalMinutes * 60;
    body = compose('%02d:%02d', [totalMinutes, sr]);
  elseif any(fmt == '.')
    ms = round(fraction * 1000);
    body = compose('%02d:%02d:%02d.%03d', [h, mi, s, ms]);
  else
    body = compose('%02d:%02d:%02d', [h, mi, s]);
  end
  out(finite) = signText + body;
end
%=============================================================================
