%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [lat, lon] = ellipse1(lat0, lon0, ellipse, offset, npts)
  % Points on a small ellipse on a sphere.
  %   [lat, lon] = ellipse1(lat0, lon0, [semimajor ecc])
  %   [lat, lon] = ellipse1(lat0, lon0, [semimajor ecc], offset)
  %   [lat, lon] = ellipse1(lat0, lon0, [semimajor ecc], offset, npts)
  % semimajor is a spherical distance (degrees of arc), ecc the eccentricity,
  % offset the azimuth (degrees) of the semimajor axis. Angles are in degrees.
  narginchk(3, 5);
  if nargin < 4 || isempty(offset)
    offset = 0;
  end
  if nargin < 5 || isempty(npts)
    npts = 100;
  end
  if numel(ellipse) ~= 2
    error('Nelson:ellipse1:invalidEllipse', 'ELLIPSE must be [semimajor eccentricity].');
  end
  a = ellipse(1);
  ecc = ellipse(2);
  b = a * sqrt(1 - ecc ^ 2);
  % Geographic azimuth of each boundary point, and the corresponding spherical
  % range given by the ellipse radius measured from its center.
  az = linspace(0, 360, npts);
  theta = deg2rad(az - offset);
  r = (a * b) ./ sqrt((b * cos(theta)) .^ 2 + (a * sin(theta)) .^ 2);
  [lat, lon] = reckon(lat0, lon0, r(:), az(:));
end
%=============================================================================
