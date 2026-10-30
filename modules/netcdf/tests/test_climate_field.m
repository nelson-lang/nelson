%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
ncClimateFigureVisible = 'off';
ncClimateAnimationEnabled = true;
ncClimateFramePause = 0;
ncClimateAnimationContinuous = false;
ncClimateMonths = 6;
run([modulepath('netcdf', 'root'), '/examples/climate_field.m']);

assert_istrue(isgraphics(ncClimateFigure));
assert_istrue(isgraphics(ncClimateImage));
% The field was written to and read back from a NetCDF file.
assert_istrue(isfile(ncClimateFile));
assert_isequal(size(ncClimateData), [ncClimateLat ncClimateLon ...
  ncClimateMonths]);
% The anomaly field has both warm and cold regions.
assert_istrue(max(ncClimateData(:)) > 0);
assert_istrue(min(ncClimateData(:)) < 0);
delete(ncClimateFigure);
drawnow();
%=============================================================================
