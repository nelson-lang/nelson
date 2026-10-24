%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--FILE WATCHER REQUIRED-->
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'File watcher service is not available in the portable runtime.');
% A burst of files written to a watched directory between two lookups must be
% visible to the lookup that follows it, whatever the order of the writes and
% however many change notifications the burst produced.
clear fw_burst_a fw_burst_b fw_burst_c
path_test = [tempdir(), createGUID()];
mkdir(path_test);
addpath(path_test);
asserts.checkerror('fw_burst_a()', sprintf(_('Undefined variable or function: %s'), 'fw_burst_a'));
for k = 1:24
  filewrite([path_test, '/noise_', int2str(k), '.txt'], "noise");
end
names = {'fw_burst_a', 'fw_burst_b', 'fw_burst_c'};
for k = 1:numel(names)
  M = {['function r = ', names{k}, '()']; ['r = ', int2str(k), ';']; 'end'};
  filewrite([path_test, '/', names{k}, '.m'], M);
  filewrite([path_test, '/', names{k}, '.txt'], M);
end
asserts.isequal(fw_burst_a(), 1);
asserts.isequal(fw_burst_b(), 2);
asserts.isequal(fw_burst_c(), 3);
clear fw_burst_a fw_burst_b fw_burst_c
rmpath(path_test);
rmdir(path_test, 's');
asserts.checkerror('fw_burst_a()', sprintf(_('Undefined variable or function: %s'), 'fw_burst_a'));
%=============================================================================
