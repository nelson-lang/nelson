%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
directory = tempname();
mkdir(directory);
source = [directory, '/definition_storage_probe.m'];
lines = {
  'function [value, callback] = definition_storage_probe(x)'
  '  arguments'
  '    x (1, 1) double'
  '  end'
  '  value = local_twice(x);'
  '  callback = @increment;'
  '  function result = increment()'
  '    x = x + 1;'
  '    result = x;'
  '  end'
  'end'
  'function result = local_twice(value)'
  '  result = 2 * value;'
  'end'
};
fid = fopen(source, 'wt');
for k = 1:numel(lines)
  fprintf(fid, '%s\n', lines{k});
end
fclose(fid);
addpath(directory);
asserts.isequal(nargin('definition_storage_probe'), 1);
asserts.isequal(nargout('definition_storage_probe'), 2);
[value, retained] = definition_storage_probe(3);
asserts.isequal(value, 6);
asserts.isequal(retained(), 4);
clear definition_storage_probe;
[value, reloaded] = definition_storage_probe(10);
asserts.isequal(value, 20);
asserts.isequal(reloaded(), 11);
asserts.isequal(retained(), 5);
clear definition_storage_probe;
rmpath(directory);
asserts.isequal(retained(), 6);
asserts.isequal(reloaded(), 12);
clear retained reloaded;
rmdir(directory, 's');
