%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [lat2, lon2] = reckon(lat1, lon1, arclen, az)
  % Point at a given spherical range and azimuth from a start point.
  %   [lat2, lon2] = reckon(lat1, lon1, arclen, az)
  % All angles are in degrees; arclen is a spherical distance in degrees of arc.
  narginchk(4, 4);
  lat1r = deg2rad(lat1);
  lon1r = deg2rad(lon1);
  d = deg2rad(arclen);
  azr = deg2rad(az);
  lat2r = asin(sin(lat1r) .* cos(d) + cos(lat1r) .* sin(d) .* cos(azr));
  lon2r = lon1r + atan2(sin(azr) .* sin(d) .* cos(lat1r), ...
    cos(d) - sin(lat1r) .* sin(lat2r));
  lat2 = rad2deg(lat2r);
  lon2 = rad2deg(lon2r);
end
%=============================================================================
