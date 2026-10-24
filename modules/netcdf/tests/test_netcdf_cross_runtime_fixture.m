%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
fixtureDirectory = [nelsonroot(), '/modules/netcdf/tests/'];
sourceFile = [fixtureDirectory, 'reference_source_basic.nc'];
temperature = ncread(sourceFile, 'temperature');
asserts.isequal(temperature(:)', 1:6);
asserts.isequal(ncreadatt(sourceFile, '/', 'title'), 'cross runtime source');
asserts.isequal(ncreadatt(sourceFile, 'temperature', 'units'), 'degree');
quality = ncread(sourceFile, 'quality');
asserts.isequal(double(quality(:)'), [7 8 9]);
asserts.isequal(double(ncreadatt(sourceFile, 'quality', 'flag')), 42);
info = ncinfo(sourceFile);
asserts.isequal(info.NumVariables, 2);
asserts.isequal(info.NumDimensions, 2);
targetFile = [fixtureDirectory, 'nelson_source_basic.nc'];
signal = ncread(targetFile, 'signal');
asserts.isequal(signal(:)', [10 20 30 40]);
asserts.isequal(ncreadatt(targetFile, '/', 'title'), 'cross runtime target');
asserts.isequal(ncreadatt(targetFile, 'signal', 'units'), 'volt');
%=============================================================================