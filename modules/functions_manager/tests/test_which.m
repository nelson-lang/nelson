%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
R = which('userpath', '-module');
REF = {'functions_manager'};
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(isempty(R{1}) || isequal(R, REF));
else
  asserts.isequal(R, REF);
end
%=============================================================================
R = which('userpath');
asserts.isequal(which('userpath.m'), R);
asserts.isequal(which('userpath.M'), R);
R_MODULE = which('userpath.m', '-module');
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(isempty(R_MODULE{1}) || isequal(R_MODULE, {'functions_manager'}));
else
  asserts.isequal(R_MODULE, {'functions_manager'});
end
R_ALL = which('userpath.m', '-all');
asserts.istrue(iscell(R_ALL));
asserts.istrue(~isempty(R_ALL));
asserts.isequal(R_ALL{1}, R);
%=============================================================================
R = which('exist', '-module');
REF = {'core'};
asserts.isequal(R, REF);
%=============================================================================
R = which('exist', '-all');
asserts.istrue(startsWith(R, modulepath('core')));
%=============================================================================
matrix = common_functions_matrix();
names = {matrix.name};
asserts.isequal(length(names), length(unique(names)));
areas = unique({matrix.area});
expectedAreas = {'arrays', 'containers', 'data_analysis', 'data_types', ...
  'elementary', 'files', 'language', 'linear_algebra', 'numeric', ...
  'polynomials', 'random', 'statistics', 'storage', 'streams', 'text', 'time', ...
  'trigonometry', 'types'};
for k = 1:length(expectedAreas)
  asserts.istrue(any(strcmp(areas, expectedAreas{k})), ['Missing compatibility area: ', expectedAreas{k}]);
end
[builtinFunctions, macroFunctions] = what();
for k = 1:length(matrix)
  entry = matrix(k);
  asserts.isequal(entry.status, 'supported');
  asserts.isequal(entry.tier, 'common');
  name = entry.name;
  isPublic = any(strcmp(builtinFunctions, name)) || any(strcmp(macroFunctions, name));
  if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()) && ~isPublic
    continue
  end
  asserts.istrue(isPublic, ['Missing public compatibility function: ', name]);
  modules = which(name, '-module');
  asserts.istrue(iscell(modules), ['which(''', name, ''', ''-module'') must return a cell.']);
  asserts.istrue(~isempty(modules), ['Missing module for compatibility function: ', name]);
  location = which(name);
  asserts.istrue(ischar(location), ['which(''', name, ''') must return a character row vector.']);
  asserts.istrue(~isempty(location), ['Missing location for compatibility function: ', name]);
  allLocations = which(name, '-all');
  asserts.istrue(iscell(allLocations), ['which(''', name, ''', ''-all'') must return a cell.']);
  asserts.istrue(~isempty(allLocations), ['Missing locations for compatibility function: ', name]);
end
R_MODULE = which('userpath', '-module');
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(isempty(R_MODULE{1}) || isequal(R_MODULE, {'functions_manager'}));
else
  asserts.isequal(R_MODULE, {'functions_manager'});
end
asserts.isequal(which('exist', '-module'), {'core'});
existLocations = which('exist', '-all');
asserts.istrue(iscell(existLocations));
asserts.istrue(~isempty(existLocations));
asserts.istrue(startsWith(existLocations{1}, modulepath('core')));
%=============================================================================
