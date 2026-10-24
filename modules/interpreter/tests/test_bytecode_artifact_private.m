%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if iswasm()
  skip_testsuite('bytecode artifact engine is not available in the portable WebAssembly runtime.');
end
%=============================================================================

directory = tempname();
mkdir(directory);
mkdir(fullfile(directory, 'private'));
mkdir(fullfile(directory, '+ncpkg'));
cleanup = onCleanup(@() rmdir(directory, 's'));
write_lines(fullfile(directory, 'nc_private_entry.m'), {
  'function value = nc_private_entry(x)'
  'value = nc_private_leaf(x) + ncpkg.twice(x);'
  'end'});
write_lines(fullfile(directory, 'private', 'nc_private_leaf.m'), {
  'function value = nc_private_leaf(x)'
  'value = 3 * x;'
  'end'});
write_lines(fullfile(directory, '+ncpkg', 'twice.m'), {
  'function value = twice(x)'
  'value = 2 * x;'
  'end'});
names = {'nc_private_entry', 'private/nc_private_leaf', '+ncpkg/twice'};
for k = 1:numel(names)
  source = fullfile(directory, [names{k}, '.m']);
  __bytecode_artifact__('compile', source, fullfile(directory, [names{k}, '.nbc']));
  delete(source);
end
addpath(directory);
restore = onCleanup(@() rmpath(directory));
asserts.isequal(nc_private_entry(4), 20);
asserts.isequal(nc_private_entry(5), 25);
asserts.isequal(ncpkg.twice(6), 12);
asserts.isequal(exist('nc_private_leaf', 'file'), 0);
clear restore;
clear cleanup;

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  for k = 1:numel(lines)
    fprintf(fid, '%s\n', lines{k});
  end
  fclose(fid);
end
