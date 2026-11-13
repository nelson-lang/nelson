%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unzip error paths and file attributes.
%=============================================================================
TESTS = [nelsonroot(), '/modules/file_archiver/tests/'];
WORK = [tempdir(), createGUID()];
mkdir(WORK);
%=============================================================================
% A missing archive is an error and creates no destination folder.
DEST = [WORK, '/missing'];
errorId = '';
try
  unzip([WORK, '/nope.zip'], DEST);
catch e
  errorId = e.identifier;
end
asserts.isequal(errorId, 'Nelson:file_archiver:cannotReadFile');
asserts.isfalse(isfolder(DEST));
%=============================================================================
% A wrong checksum is reported.
DEST = [WORK, '/crc'];
cmd = 'unzip([TESTS, ''test_unzip_bad_crc.zip''], DEST)';
asserts.checkerror(cmd, _('Cannot read data.'), 'Nelson:unzip:cannotReadData');
%=============================================================================
% Unix permissions: execute bits restored on POSIX, nothing applied on Windows.
DEST = [WORK, '/unix'];
R = unzip([TESTS, 'test_unzip_unix_mode.zip'], DEST);
asserts.isequal(R, {[DEST, '/run.sh'], [DEST, '/data.txt']});
asserts.isequal(fileread([DEST, '/data.txt']), sprintf('data\n'));
if ispc()
  for k = 1:numel(R)
    [status, msg] = system(['attrib "', strrep(R{k}, '/', '\'), '"']);
    asserts.isequal(status, 0);
    % attrib prints the flags, then the path starting with its drive letter.
    flags = msg(1:find(msg == ':', 1) - 2);
    asserts.isfalse(any(ismember('RHSP', flags)));
  end
else
  [status, ~] = system(['test -x "', DEST, '/run.sh"']);
  asserts.isequal(status, 0);
  [status, ~] = system(['test -x "', DEST, '/data.txt"']);
  asserts.isfalse(status == 0);
end
%=============================================================================
cd(tempdir());
rmdir(WORK, 's');
%=============================================================================
