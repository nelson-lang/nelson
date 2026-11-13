%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% tar.xz and tar.bz2 made by the system tar are read through libarchive.
skip_testsuite(~ispc(), 'Windows builds always have libarchive and bsdtar.');
WORK = tempname();
mkdir(WORK);
cleanup = onCleanup(@() rmdir(WORK, 's'));
filewrite([WORK, '/a.txt'], 'archived text');
cases = {'J', '.tar.xz', 'xz'; 'j', '.tar.bz2', 'bzip2'};
for k = 1:size(cases, 1)
  archive = [WORK, '/out', cases{k, 2}];
  [status, output] = system(['cd /d "', WORK, '" && "', getenv('SystemRoot'), ...
    '\System32\tar.exe" -c', cases{k, 1}, 'f "', archive, '" a.txt']);
  asserts.isequal(status, 0, output);
  t = nelson.io.TarArchive(archive);
  asserts.isequal(char(t.Compression), cases{k, 3});
  asserts.isequal(t.readtext('a.txt'), 'archived text');
  t.close();
  names = untar(archive, [WORK, '/x', cases{k, 1}]);
  asserts.isequal(fileread(names{1}), 'archived text');
end
%=============================================================================
