%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% svg = NFlow.internal.mslIconSvg(block, palette, name)
%
% Renders one Modelica icon (the data extracted by
% docs/tools/extract_msl_icons.py into libraries/msl-icons.json) as an SVG
% laid out exactly like the OpenModelica documentation icons: one model unit
% per pixel, the viewBox is the bounding box of the graphics (text extents
% included) plus half a stroke, the y axis is flipped by a group matrix, the
% strokes are 13 px per mm of Modelica thickness, the texts take 80 % of their
% extent height, and gradient / hatch fills are flattened to their fill
% colour. Only the colours differ: a card tinted by the nflow domain fills the
% viewBox, strokes and texts use the domain ink, fills are mapped to white /
% tint / ink by luminance (light grey strokes to the tint).
%
% block    struct: e (class extent 2x2), g (graphics, cell or struct array)
% palette  struct: bg, ink, tint (CSS colours)
% name     block name, substituted for the Modelica "%name" label
%=============================================================================
function svg = mslIconSvg(block, palette, name)
  items = ms_items(block.g);
  [lo, hi] = ms_bounds(block.e, items);
  ctx = struct('ink', palette.ink, 'tint', palette.tint, 'name', name);
  body = '';
  for k = 1:numel(items)
    body = [body, ms_prim(items{k}, ctx)]; %#ok<AGROW>
  end
  w = hi(1) - lo(1); h = hi(2) - lo(2);
  dash = '';
  if isfield(palette, 'dash') && palette.dash
    dash = ' stroke-dasharray="21 13"';
  end
  svg = [...
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="%s %s %s %s">', ...
      ms_num(w), ms_num(h), ms_num(lo(1)), ms_num(-hi(2)), ms_num(w), ms_num(h)), newline, ...
    sprintf('  <rect x="%s" y="%s" width="%s" height="%s" rx="%s" fill="%s" stroke="%s" stroke-width="3.25"%s/>', ...
      ms_num(lo(1)), ms_num(-hi(2)), ms_num(w), ms_num(h), ms_num(0.19 * min(w, h)), palette.bg, palette.tint, dash), newline, ...
    '  <g transform="matrix(1 0 0 -1 0 0)">', newline, ...
    body, ...
    '  </g>', newline, ...
    '</svg>', newline];
end
%=============================================================================
function items = ms_items(g)
  % jsondecode gives a struct array for uniform objects, a cell otherwise.
  if iscell(g)
    items = g;
  else
    items = cell(1, numel(g));
    for k = 1:numel(g)
      items{k} = g(k);
    end
  end
end
function s = ms_num(v)
  s = sprintf('%.5g', v);
  if strcmp(s, '-0')
    s = '0';
  end
end
function v = ms_field(p, f, default)
  v = default;
  if isfield(p, f) && ~isempty(p.(f))
    v = p.(f);
  end
end
function lw = ms_width(p)
  lw = ms_field(p, 'w', 0.25) * 13;
end
%=============================================================================
function [lo, hi] = ms_bounds(extent, items)
  % Bounding box of the class coordinate system and of every primitive (points
  % or extent, in its own origin / rotation), padded by the widest stroke, as
  % the documentation icons.
  lo = min(extent); hi = max(extent); pad = 0;
  for k = 1:numel(items)
    p = items{k};
    if isfield(p, 'p')
      P = p.p;
    else
      e = p.e;
      P = [e(1, 1), e(1, 2); e(2, 1), e(1, 2); e(2, 1), e(2, 2); e(1, 1), e(2, 2)];
    end
    o = ms_field(p, 'o', [0, 0]); o = o(:)';
    a = ms_field(p, 'r', 0) * pi / 180;
    R = [cos(a), -sin(a); sin(a), cos(a)];
    for i = 1:size(P, 1)
      q = o + (R * P(i, :)')';
      lo = min(lo, q); hi = max(hi, q);
    end
    if ~strcmp(ms_field(p, 'lp', 'Solid'), 'None') && ~strcmp(p.t, 'text')
      pad = max(pad, ms_width(p) / 2 + 0.25);
    end
  end
  pad = max(pad, 3.25);
  lo = lo - pad; hi = hi + pad;
end
%=============================================================================
function s = ms_prim(p, ctx)
  switch p.t
    case 'line'
      s = ms_line(p, ctx);
    case 'rectangle'
      s = ms_rect(p, ctx);
    case 'ellipse'
      s = ms_ellipse(p, ctx);
    case 'polygon'
      s = ms_polygon(p, ctx);
    case 'text'
      s = ms_text(p, ctx);
    otherwise
      s = '';
  end
  if isempty(s)
    return
  end
  o = ms_field(p, 'o', [0, 0]); r = ms_field(p, 'r', 0);
  if any(o ~= 0) || r ~= 0
    s = [sprintf('    <g transform="translate(%s %s) rotate(%s)">', ms_num(o(1)), ms_num(o(2)), ...
      ms_num(r)), newline, s, '    </g>', newline];
  end
end
%=============================================================================
function a = ms_stroke(p, ctx)
  % stroke attributes: domain ink (tint for the light grey secondary strokes),
  % 13 px per mm, Modelica dash patterns.
  lp = ms_field(p, 'lp', 'Solid');
  if strcmp(lp, 'None')
    a = 'stroke="none"';
    return
  end
  col = ctx.ink;
  c = ms_field(p, 'c', ms_field(p, 'lc', [0, 0, 0]));
  if 0.299 * c(1) + 0.587 * c(2) + 0.114 * c(3) >= 160
    col = ctx.tint;
  end
  a = sprintf('stroke="%s" stroke-width="%s"', col, ms_num(ms_width(p)));
  switch lp
    case 'Dash'
      a = [a, ' stroke-dasharray="13 6.5"'];
    case 'Dot'
      a = [a, ' stroke-dasharray="3.25 6.5"'];
    case 'DashDot'
      a = [a, ' stroke-dasharray="13 6.5 3.25 6.5"'];
    case 'DashDotDot'
      a = [a, ' stroke-dasharray="13 6.5 3.25 6.5 3.25 6.5"'];
  end
end
%=============================================================================
function f = ms_fill(p, ctx)
  % fill mapped to the domain palette by the Modelica fill colour luminance;
  % gradients and hatches are flattened to their fill colour.
  if strcmp(ms_field(p, 'fp', 'None'), 'None')
    f = 'none';
    return
  end
  fc = ms_field(p, 'fc', [255, 255, 255]);
  lum = 0.299 * fc(1) + 0.587 * fc(2) + 0.114 * fc(3);
  if lum >= 240
    f = '#FFFFFF';
  elseif lum < 128
    f = ctx.ink;
  else
    f = ctx.tint;
  end
end
%=============================================================================
function s = ms_points(P)
  parts = cell(1, size(P, 1));
  for k = 1:size(P, 1)
    parts{k} = sprintf('%s,%s', ms_num(P(k, 1)), ms_num(P(k, 2)));
  end
  s = strjoin(parts, ' ');
end
function d = ms_bezier(P, closed)
  % Modelica Smooth.Bezier: quadratic curves through the segment midpoints.
  n = size(P, 1);
  if closed
    P = [P; P(1, :); P(2, :)];
    m = (P(1, :) + P(2, :)) / 2;
    d = sprintf('M%s %s', ms_num(m(1)), ms_num(m(2)));
    for k = 2:n + 1
      m = (P(k, :) + P(k + 1, :)) / 2;
      d = [d, sprintf(' Q%s %s %s %s', ms_num(P(k, 1)), ms_num(P(k, 2)), ms_num(m(1)), ms_num(m(2)))]; %#ok<AGROW>
    end
    d = [d, ' Z'];
  else
    d = sprintf('M%s %s', ms_num(P(1, 1)), ms_num(P(1, 2)));
    for k = 2:n - 2
      m = (P(k, :) + P(k + 1, :)) / 2;
      d = [d, sprintf(' Q%s %s %s %s', ms_num(P(k, 1)), ms_num(P(k, 2)), ms_num(m(1)), ms_num(m(2)))]; %#ok<AGROW>
    end
    d = [d, sprintf(' Q%s %s %s %s', ms_num(P(n - 1, 1)), ms_num(P(n - 1, 2)), ms_num(P(n, 1)), ms_num(P(n, 2)))];
  end
end
%=============================================================================
function s = ms_line(p, ctx)
  P = p.p;
  if strcmp(ms_field(p, 's', 'None'), 'Bezier') && size(P, 1) >= 3
    s = sprintf('    <path d="%s" fill="none" %s stroke-linecap="round" stroke-linejoin="round"/>', ...
      ms_bezier(P, false), ms_stroke(p, ctx));
  else
    s = sprintf('    <polyline points="%s" fill="none" %s stroke-linecap="round" stroke-linejoin="round"/>', ...
      ms_points(P), ms_stroke(p, ctx));
  end
  s = [s, newline];
  arrows = ms_field(p, 'a', {'None', 'None'});
  if iscell(arrows) && numel(arrows) == 2
    sz = ms_field(p, 'as', 3);
    s = [s, ms_arrow(arrows{1}, P(1, :), P(2, :), sz, ctx), ms_arrow(arrows{2}, P(end, :), P(end - 1, :), sz, ctx)];
  end
end
function s = ms_arrow(kind, tip, from, sz, ctx)
  s = '';
  if strcmp(kind, 'None')
    return
  end
  len = sz * 4;
  d = tip - from; L = max(norm(d), 1e-9); u = d / L; nv = [-u(2), u(1)];
  b = tip - u * len; l = b + nv * len * 0.4; r = b - nv * len * 0.4;
  switch kind
    case 'Open'
      s = sprintf('    <polyline points="%s" fill="none" stroke="%s" stroke-width="3.25"/>', ...
        ms_points([l; tip; r]), ctx.ink);
    case 'Half'
      s = sprintf('    <polygon points="%s" fill="%s"/>', ms_points([tip; l; b]), ctx.ink);
    otherwise
      s = sprintf('    <polygon points="%s" fill="%s"/>', ms_points([tip; l; r]), ctx.ink);
  end
  s = [s, newline];
end
%=============================================================================
function s = ms_rect(p, ctx)
  e = p.e;
  lo = min(e); hi = max(e);
  rad = ms_field(p, 'rad', 0);
  rx = '';
  if rad > 0
    rx = sprintf(' rx="%s" ry="%s"', ms_num(rad), ms_num(rad));
  end
  s = sprintf('    <rect x="%s" y="%s" width="%s" height="%s"%s fill="%s" %s/>%s', ...
    ms_num(lo(1)), ms_num(lo(2)), ms_num(hi(1) - lo(1)), ms_num(hi(2) - lo(2)), rx, ...
    ms_fill(p, ctx), ms_stroke(p, ctx), newline);
end
%=============================================================================
function s = ms_ellipse(p, ctx)
  e = p.e;
  lo = min(e); hi = max(e);
  c = (lo + hi) / 2; rx = (hi(1) - lo(1)) / 2; ry = (hi(2) - lo(2)) / 2;
  f = ms_fill(p, ctx);
  a0 = ms_field(p, 'a0', 0); a1 = ms_field(p, 'a1', 360);
  if abs(a1 - a0) >= 360
    s = sprintf('    <ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="%s" %s/>%s', ...
      ms_num(c(1)), ms_num(c(2)), ms_num(rx), ms_num(ry), f, ms_stroke(p, ctx), newline);
    return
  end
  p0 = c + [rx * cos(a0 * pi / 180), ry * sin(a0 * pi / 180)];
  p1 = c + [rx * cos(a1 * pi / 180), ry * sin(a1 * pi / 180)];
  large = double(abs(a1 - a0) > 180);
  sweep = double(a1 > a0);
  d = sprintf('M%s %s A%s %s 0 %d %d %s %s', ms_num(p0(1)), ms_num(p0(2)), ms_num(rx), ms_num(ry), ...
    large, sweep, ms_num(p1(1)), ms_num(p1(2)));
  cl = ms_field(p, 'cl', 'Chord');
  if strcmp(cl, 'Radial')
    d = [d, sprintf(' L%s %s Z', ms_num(c(1)), ms_num(c(2)))];
  elseif strcmp(cl, 'Chord')
    d = [d, ' Z'];
  end
  s = sprintf('    <path d="%s" fill="%s" %s/>%s', d, f, ms_stroke(p, ctx), newline);
end
%=============================================================================
function s = ms_polygon(p, ctx)
  P = p.p;
  f = ms_fill(p, ctx);
  if strcmp(ms_field(p, 's', 'None'), 'Bezier') && size(P, 1) >= 3
    s = sprintf('    <path d="%s" fill="%s" %s stroke-linejoin="round"/>%s', ms_bezier(P, true), f, ...
      ms_stroke(p, ctx), newline);
  else
    s = sprintf('    <polygon points="%s" fill="%s" %s stroke-linejoin="round"/>%s', ms_points(P), f, ...
      ms_stroke(p, ctx), newline);
  end
end
%=============================================================================
function s = ms_text(p, ctx)
  e = p.e;
  lo = min(e); hi = max(e);
  txt = strrep(p.s, '%name', ctx.name);
  txt = strrep(strrep(strrep(txt, '&', '&amp;'), '<', '&lt;'), '>', '&gt;');
  fs = ms_field(p, 'fs', 0);
  if fs > 0
    fs = fs * 13;
  else
    fs = 0.8 * (hi(2) - lo(2));
  end
  extra = '';
  st = ms_field(p, 'st', {});
  if iscell(st)
    if any(strcmp(st, 'Bold'))
      extra = [extra, ' font-weight="bold"'];
    end
    if any(strcmp(st, 'Italic'))
      extra = [extra, ' font-style="italic"'];
    end
  end
  switch ms_field(p, 'h', 'Center')
    case 'Left'
      anchor = 'start'; x = lo(1);
    case 'Right'
      anchor = 'end'; x = hi(1);
    otherwise
      anchor = 'middle'; x = (lo(1) + hi(1)) / 2;
  end
  s = sprintf(['    <g transform="translate(%s,%s) scale(1,-1)"><text x="0" y="0" text-anchor="%s" ', ...
    'dominant-baseline="central" font-size="%s" fill="%s"%s>%s</text></g>%s'], ...
    ms_num(x), ms_num((lo(2) + hi(2)) / 2), anchor, ms_num(fs), ctx.ink, extra, txt, newline);
end
%=============================================================================
