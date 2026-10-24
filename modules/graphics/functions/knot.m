%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = knot(varargin)
  narginchk(0, 1);
  nargoutchk(0, 3);
  if (nargin == 1)
    ax = varargin{1};
  else
    ax = [];
  end
  % --- Trefoil knot tube ------------------------------------------------
  % A tube is swept around a parametric space curve using its Frenet frame
  % (tangent / principal normal / binormal). The curve is built from a
  % single 1-periodic generating function evaluated with three phase
  % shifts, which produces the trefoil knot (original construction by
  % R. Rostamian, 1991). Because the generator is analytic, its first and
  % second derivatives are known in closed form, so the frame is exact.
  m = 20;               % facets around each circular cross-section
  n = 60;               % sections along the tube
  R = 0.75;             % radius of the swept tube
  q = floor(n / 3);     % phase shift giving the knot its 3-fold symmetry
  t = (0:n) / n;
  % Generating function f0 and its derivatives f1 = f0', f2 = f0''.
  a = 2;
  b = 3;
  c = 1.5;
  q1 = 2;
  q2 = 4;
  f0 = sin(q1 * pi * t) + a * sin(q2 * pi * t) - b * cos(4 * pi * t) / 2 + c * sin(6 * pi * t);
  f1 = q1 * pi * cos(q1 * pi * t) + a * q2 * pi * cos(q2 * pi * t) + b * 4 * pi * sin(4 * pi * t) / 2 + c * 6 * pi * cos(6 * pi * t);
  f2 = -(q1 * pi)^2 * sin(q1 * pi * t) - a * (q2 * pi)^2 * sin(q2 * pi * t) + b * (4 * pi)^2 * cos(4 * pi * t) / 2 - c * (6 * pi)^2 * sin(6 * pi * t);
  % Extend each generator over two periods so the shifted slices below stay
  % in range.
  f0 = [f0(1:n), f0(1:n)];
  f1 = [f1(1:n), f1(1:n)];
  f2 = [f2(1:n), f2(1:n)];
  % Center line, velocity and acceleration, each coordinate being the same
  % generator read with a phase shift of 0, q and 2q.
  x10 = f0(1:n + 1);   x20 = f0(q + 1:q + n + 1);   x30 = f0(2 * q + 1:2 * q + n + 1);
  x11 = f1(1:n + 1);   x21 = f1(q + 1:q + n + 1);   x31 = f1(2 * q + 1:2 * q + n + 1);
  x12 = f2(1:n + 1);   x22 = f2(q + 1:q + n + 1);   x32 = f2(2 * q + 1:2 * q + n + 1);
  % Frenet frame from the velocity and acceleration.
  speed = sqrt(x11 .^ 2 + x21 .^ 2 + x31 .^ 2);
  velacc = x11 .* x12 + x21 .* x22 + x31 .* x32;
  normal1 = speed .^ 2 .* x12 - velacc .* x11;
  normal2 = speed .^ 2 .* x22 - velacc .* x21;
  normal3 = speed .^ 2 .* x32 - velacc .* x31;
  nn = sqrt(normal1 .^ 2 + normal2 .^ 2 + normal3 .^ 2);
  unitnormal1 = normal1 ./ nn;
  unitnormal2 = normal2 ./ nn;
  unitnormal3 = normal3 ./ nn;
  binormal1 = (x21 .* unitnormal3 - x31 .* unitnormal2) ./ speed;
  binormal2 = (x31 .* unitnormal1 - x11 .* unitnormal3) ./ speed;
  binormal3 = (x11 .* unitnormal2 - x21 .* unitnormal1) ./ speed;
  % Sweep the circular cross-section along the frame. Each output is
  % (m + 1) x (n + 1): rows run around the tube, columns run along it.
  s = (0:m)' * (2 * pi / m);
  x = ones(m + 1, 1) * x10 + R * (cos(s) * unitnormal1 + sin(s) * binormal1);
  y = ones(m + 1, 1) * x20 + R * (cos(s) * unitnormal2 + sin(s) * binormal2);
  z = ones(m + 1, 1) * x30 + R * (cos(s) * unitnormal3 + sin(s) * binormal3);

  if (nargout == 0)
    % Color varies -1..1 along the length of the tube.
    color = ones(m + 1, 1) * ((0:n) * 2 / n - 1);
    ax = newplot(ax);
    surf(x, y, z, color, 'Parent', ax);
    % Render the tube as a smooth, lit surface with the axes hidden, framed
    % so that it fills the figure.
    shading(ax, 'interp');
    light('Parent', ax);
    lighting(ax, 'gouraud');
    view(ax, 2);
    axis(ax, 'equal');
    axis(ax, 'off');
    axis(ax, 'vis3d');
    axis(ax, 'tight');
  else
    varargout{1} = x;
    varargout{2} = y;
    varargout{3} = z;
  end
end
%=============================================================================
