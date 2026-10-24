%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = residue(varargin)
  narginchk(2, 3);
  nargoutchk(0, 3);
  if nargin == 2
    [r, p, k] = residue_expansion(varargin{1}, varargin{2});
    varargout{1} = r;
    if nargout >= 2
      varargout{2} = p;
    end
    if nargout >= 3
      varargout{3} = k;
    end
  else
    [b, a] = residue_recombine(varargin{1}, varargin{2}, varargin{3});
    varargout{1} = b;
    if nargout >= 2
      varargout{2} = a;
    end
  end
end
%=============================================================================
function [r, p, k] = residue_expansion(b, a)
  b = strip_leading_zeros(full(b(:)).');
  a = strip_leading_zeros(full(a(:)).');
  if isempty(a)
    error('Nelson:residue:denominatorMustBeNonzero', 'Denominator polynomial must be nonzero.');
  end
  la = numel(a);
  lb = numel(b);
  if lb >= la
    [k, b] = deconv(b, a);
    k = k(:).';
    b = strip_leading_zeros(b);
  else
    k = [];
  end
  n = la - 1;
  if n == 0
    r = zeros(0, 1);
    p = zeros(0, 1);
    return
  end
  tol = residue_tolerance();
  [p, mult] = cluster_poles(roots(a), tol);
  brhs = pad_or_trim_column(b(:), n);
  M = zeros(n, n) + 0 * (brhs(1) + p(1));
  for i = 1:n
    q = a(1) * poly_without(p, i, mult(i), tol);
    M(:, i) = pad_or_trim_column(q(:), n);
  end
  r = M \ brhs;
  if isreal(a) && isreal(b)
    r = drop_negligible_imaginary(r);
    p = drop_negligible_imaginary(p);
  end
end
%=============================================================================
function [b, a] = residue_recombine(r, p, k)
  r = full(r(:));
  p = full(p(:));
  n = numel(p);
  if numel(r) ~= n
    error('Nelson:dimagree:rPNumberElements', 'R and P must have the same number of elements.');
  end
  tol = residue_tolerance();
  mult = consecutive_multiplicity(p, tol);
  if n == 0
    a = 1;
  else
    a = poly(p.');
  end
  b = 0;
  for i = 1:n
    b = add_polynomials(b, r(i) * poly_without(p, i, mult(i), tol));
  end
  if ~isempty(k)
    b = add_polynomials(b, conv(full(k(:)).', a));
  end
  if isreal(r) && isreal(p) && isreal_or_empty(k)
    b = drop_negligible_imaginary(b);
    a = drop_negligible_imaginary(a);
  end
end
%=============================================================================
function t = residue_tolerance()
  t = 1e-3;
end
%=============================================================================
function v = strip_leading_zeros(v)
  v = v(:).';
  idx = find(v ~= 0, 1, 'first');
  if isempty(idx)
    v = zeros(1, 0);
  else
    v = v(idx:end);
  end
end
%=============================================================================
function [pcl, mult] = cluster_poles(p, tol)
  % Poles are ordered by decreasing magnitude with a stable tie break, then
  % equal poles are grouped together and replaced by their average.
  p = p(:);
  n = numel(p);
  [ignore, ord] = sortrows([-abs(p), (1:n)']); %#ok
  p = p(ord);
  used = false(n, 1);
  pcl = zeros(0, 1);
  mult = zeros(0, 1);
  for i = 1:n
    if used(i)
      continue
    end
    used(i) = true;
    idxs = i;
    for j = i + 1:n
      if ~used(j) && abs(p(j) - p(i)) <= tol * max(1, abs(p(i)))
        idxs(end + 1) = j;
        used(j) = true;
      end
    end
    m = numel(idxs);
    pm = mean(p(idxs));
    for t = 1:m
      pcl(end + 1, 1) = pm;
      mult(end + 1, 1) = t;
    end
  end
end
%=============================================================================
function mult = consecutive_multiplicity(p, tol)
  n = numel(p);
  mult = zeros(n, 1);
  t = 0;
  for i = 1:n
    if i > 1 && abs(p(i) - p(i - 1)) <= tol * max(1, abs(p(i)))
      t = t + 1;
    else
      t = 1;
    end
    mult(i) = t;
  end
end
%=============================================================================
function q = poly_without(p, idx, t, tol)
  v = p(idx);
  keep = true(numel(p), 1);
  removed = 0;
  for j = 1:numel(p)
    if removed < t && abs(p(j) - v) <= tol * max(1, abs(v))
      keep(j) = false;
      removed = removed + 1;
    end
  end
  q = poly(p(keep).');
end
%=============================================================================
function w = pad_or_trim_column(v, n)
  v = v(:);
  m = numel(v);
  if m == n
    w = v;
  elseif m < n
    w = [zeros(n - m, 1); v];
  else
    w = v(m - n + 1:end);
  end
end
%=============================================================================
function s = add_polynomials(p1, p2)
  p1 = p1(:).';
  p2 = p2(:).';
  n1 = numel(p1);
  n2 = numel(p2);
  n = max(n1, n2);
  s = zeros(1, n) + 0 * (p1(1) + p2(1));
  s(n - n1 + 1:end) = p1;
  s(n - n2 + 1:end) = s(n - n2 + 1:end) + p2;
end
%=============================================================================
function v = drop_negligible_imaginary(v)
  if ~isreal(v)
    tolimag = 1e-10 * (1 + abs(real(v)));
    mask = abs(imag(v)) <= tolimag;
    if all(mask(:))
      v = real(v);
    end
  end
end
%=============================================================================
function tf = isreal_or_empty(k)
  tf = isempty(k) || isreal(k);
end
%=============================================================================
