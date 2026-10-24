%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression tests for ZIP-slip / path-traversal hardening in unzip.
% Each fixture holds a single entry whose stored name attempts to escape the
% extraction root; unzip must reject it before writing anything.
%=============================================================================
TMPDIR = tempdir();
%=============================================================================
fixtures = {'test_zipslip_dotdot.zip', ...
             'test_zipslip_absolute.zip', ...
             'test_zipslip_drive.zip', ...
             'test_zipslip_backslash.zip'};
%=============================================================================
for k = 1:numel(fixtures)
  fixture = [nelsonroot(), '/modules/file_archiver/tests/', fixtures{k}];
  % dest is a fresh directory directly under TMPDIR, so a "../" entry would
  % escape into TMPDIR itself.
  dest = [TMPDIR, createGUID()];
  mkdir(dest);
  parent_canary = [TMPDIR, 'zipslip_canary.txt'];
  if isfile(parent_canary)
    rmfile(parent_canary);
  end
  failed = false;
  msg = '';
  try
    unzip(fixture, dest);
  catch e
    failed = true;
    msg = e.message;
  end
  % extraction of a malicious entry must be rejected ...
  asserts.istrue(failed);
  asserts.istrue(contains(msg, 'Unsafe entry path'));
  % ... and nothing must have escaped the destination directory.
  asserts.isfalse(isfile(parent_canary));
  cd(tempdir());
  rmdir(dest, 's');
end
%=============================================================================
